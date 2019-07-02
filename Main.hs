module Main where

import Graphics.Gloss.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Prim

feiura :: Picture
feiura = png "sf.png"

width, height, offset, fps :: Int
width  = 600
height = 600
offset = 10
fps    = 60

window :: Display
window = InWindow
  "Maze"           -- window title
  (width, height)  -- window size
  (offset, offset) -- window position

background :: Color -- background color
background = black

wayColor :: Color
wayColor = white

tiroColor :: Color
tiroColor = blue

mkStart :: Float -> Float -> Bool -> Picture
mkStart x y ok = translate (x-1) y $ color (if (ok) then green else (light $ light $ light green)) $ rectangleSolid 1 1

mkEnd :: Float -> Float -> Bool -> Picture
mkEnd x y ok = translate (x+1) y $ color (if (ok) then red else (light $ light $ light red)) $ rectangleSolid 1 1
  
mkWay :: Float -> Float -> Picture
mkWay x y = translate x y $ color wayColor $ rectangleSolid 1 1

mkTiro :: Tirinho -> Picture
mkTiro t = translate x y $ color tiroColor $ circleSolid 2.5
  where (x,y) = pos t

type Velocidade = (Float, Float)

data Tirinho = Tirinho
  {
    pos :: Point,
    vel :: Velocidade
  } deriving Show
  
-- | Guarda o estado do jogo.
data MazeState = Game
  {
    cells :: [(Int, Int)],
    cols  :: Int,
    rows  :: Int,
    began :: Bool,
    gen   :: StdGen,
    dific :: TVar Int,
    tiros :: [Tirinho],
    mouse :: Point,
    spawn :: TVar Bool,
    perdeu:: Bool
  }
  
-- | Renderiza o estado do jogo (converte pra Picture).
render :: MazeState -> IO Picture
render game = do
  let p = perdeu game
  if (p) then (return (scale 2 2 $ feiura))
    else (do dif <- readTVarIO (dific game)
             return (pictures $
                     [translate (-(fromIntegral width / 2)) (-(fromIntegral height / 2)) $ scale 0.2 0.2 $ color white $ (text ("level: " ++ (show dif))),
                      scale xFactor yFactor $
                      translate (-((fromIntegral $ cols game) - 1)) (-((fromIntegral $ rows game)-1)) $
                      pictures $
                      [
                        mkWay (fromIntegral x) (fromIntegral y) | (x,y) <- cells game
                      ] ++ [mkStart 0 0 (began game), mkEnd (2 * ((fromIntegral $ cols game) - 1)) (2 * ((fromIntegral $ rows game) - 1)) (began game)]] ++ [renderTiros (tiros game)]))
  where xFactor = (fromIntegral $ round ((fromIntegral width / (fromIntegral (cols game)))/4.4)) * 2
        yFactor = (fromIntegral $ round ((fromIntegral width / (fromIntegral (cols game)))/4.4)) * 2

renderTiros :: [Tirinho] -> Picture
renderTiros tiros = pictures $ [mkTiro tiro | tiro <- tiros]
        
-- | Converte de vertices e arestas para malha a ser pintada
graphToMaze :: Grafo -> [(Int, Int)]
graphToMaze g = [v | (v,_) <- g] ++
  [intermediarios | ((i,j), la) <- g,
    intermediarios <- [((x+i) `div` 2, (y+j) `div` 2) | (x,y) <- la]] ++
  [v | (_,la) <- g, v <- la]

-- | Inicializa o jogo com um estado.
initialState :: StdGen -> TVar Int -> TVar Bool -> MazeState
initialState gen dif sp = Game
  { cells = graphToMaze $ gordifica $ generateMaze c r gen',
    cols  = c,
    rows  = r,
    began = False,
    gen   = gen'',
    dific = dif,
    tiros = [],
    mouse = (0, 0),
    spawn = sp,
    perdeu= False
  } where c = 5; r = 5; (gen', gen'') = split gen

-- | Atira
novoTiro :: (RandomGen t) => t -> Int -> Point-> Tirinho
novoTiro gen dific obj@(x,y) = Tirinho
  {
    pos = pos',
    vel = (vx, vy)
  }
  where
    pos'@(w, h) = if (op <= 0.25)
                    then (wid, h')
                  else if (op <= 0.5)
                    then ((-wid), h')
                  else if (op <= 0.75)
                    then (w', (-hei))
                  else (w', hei)
    (w', gen')  = randomR ((-(wid)), wid) gen
    (h', gen'') = randomR ((-(hei)), hei) gen'
    (op, _)     = randomR (0::Float,1::Float) gen''
    vx = (x-w)/t
    vy = (y-h)/t
    t  = if (t' < 1) then 1 else t'
    t' = (10 - fromIntegral dific)
    wid = fromIntegral width/2
    hei = fromIntegral height/2

-- | Detecta colisao de algum tiro com o mouse
colisaoMouse :: MazeState -> Bool
colisaoMouse game
  | foldr (||) False [(distance pos' m) <= 2.5
                     | tiro <- tirinhos,
                       let pos' = pos tiro]
  = True
  | otherwise = False
    where m = mouse game
          tirinhos = tiros game
          distance :: Point -> Point -> Float
          distance (x1, y1) (x2, y2) = sqrt (d1 + d2)
            where d1 = (x1-x2)*(x1-x2)
                  d2 = (y1-y2)*(y1-y2)
          
-- | Cria novo labirinto (progressao).
nextLevel :: MazeState -> IO MazeState
nextLevel game = do
  dif <- readTVarIO (dific game)
  let novaDific = dif + 1
  let c = 2*novaDific
  let r = 2*novaDific
  atomically (writeTVar (dific game) novaDific)
  return (game
          {
            cells = graphToMaze $ gordifica $ generateMaze c r gen',
            cols  = c,
            rows  = r,
            began = False,
            gen   = gen'',
            tiros = []
          })
    where (gen', gen'') = split (gen game)
  
-- | Multiplica grafo por 2 para ficar bonitinho
gordifica :: Grafo -> Grafo
gordifica g = [((2*i,2*j),[(2*u, 2*v) | (u,v) <- la]) | ((i,j),la) <- g]

-- | Lida com eventos 
handleEvent :: Event -> MazeState -> IO MazeState
handleEvent event game
  | EventKey (MouseButton LeftButton) Up _ _ <- event,
  perdeu game
  = do
      atomically (writeTVar (dific game) 1)
      return (initialState (gen game) (dific game) (spawn game))
  | EventKey (MouseButton RightButton) Up _ _ <- event,
  perdeu game
  = do
      atomically (writeTVar (dific game) 1)
      return (initialState (gen game) (dific game) (spawn game))
  | perdeu game -- adicionei este guard para nao modificar os outros
  = return game
  | EventKey (MouseButton LeftButton) Up _ mp@(x,y) <- event,
  ehComeco x y
  = return (game
    {
      began = (ehComeco x y) || (began game),
      mouse = mp
    })
  | EventMotion (x, y) <- event,
  began game,
  not (elem (fromPixel x y game) (cells game)) && fromPixel x y game /= ((-1),0),
  not (ehFinal x y)
  = do
      --atomically (writeTVar (dific game) 1)
      return (game { perdeu = True })--(initialState (gen game) (dific game) (spawn game))
  | EventMotion (x, y) <- event,
  began game,
  ehFinal x y
  = nextLevel game
  | EventMotion mp <- event
  = return (game
  {
    mouse = mp
  })
  | otherwise = return game
  where
    ehComeco :: Float -> Float -> Bool
    ehComeco i j = if ((fromPixel i j game) == ((-1),0)) then True else False
    ehFinal  :: Float -> Float -> Bool
    ehFinal  i j = if ((fromPixel i j game) == ((2 * ((fromIntegral $ cols game) - 1)) + 1, (2 * ((fromIntegral $ rows game) - 1)))) then True else False

-- | Traz de coordenadas de camera pra coordenadas de labirinto
-- WIDTH
fromPixelW :: Float -> MazeState -> Int
fromPixelW w game = round ((w/xFactor)+iw)
  where iw = ((fromIntegral $ cols game) - 1)
        xFactor = (fromIntegral $ round ((fromIntegral width / (fromIntegral (cols game)))/4.4)) * 2

-- | Traz de coordenadas de camera pra coordenadas de labirinto
-- HEIGHT
fromPixelH :: Float -> MazeState -> Int
fromPixelH h game = round ((h/yFactor)+ih)
  where ih = ((fromIntegral $ rows game) - 1)
        yFactor = (fromIntegral $ round ((fromIntegral width / (fromIntegral (cols game)))/4.4)) * 2
        
-- | Junta fromPixelW e fromPixelH
fromPixel :: Float -> Float -> MazeState -> (Int, Int)
fromPixel x y game = (fromPixelW x game, fromPixelH y game)

-- | Atualiza mapa
update :: Float -> MazeState -> IO MazeState
update timelapse = moveTiros timelapse

-- | Movimenta os tiros
moveTiros :: Float -> MazeState -> IO MazeState
moveTiros timelapse game
  | not (perdeu game)
  = do
      sp <- readTVarIO (spawn game)
      gen'' <- newStdGen
      dif <- readTVarIO (dific game)
      let game' =
            if (sp)
            then game { tiros = (novoTiro gen'' dif (mouse game)):tiros' }
            else game { tiros = tiros' }
      atomically (writeTVar (spawn game) False)
      if (colisaoMouse game')
        then (do
                 --atomically (writeTVar (dific game') 1)
                 return (game' { perdeu = True })) --(initialState (gen game') (dific game') (spawn game')))
        else (return game')
  | otherwise = return game
  where tiros' = filter filt (map (moveTiro timelapse) (tiros game))

filt :: Tirinho -> Bool
filt t
  | (x,y) <- (pos t),
    x > fromIntegral width/2 ||
    x < (-(fromIntegral width/2)) ||
    y > fromIntegral height/2 ||
    y < (-(fromIntegral height/2))
  = False
  | otherwise = True

-- | Move um único tirinho
moveTiro :: Float -> Tirinho -> Tirinho
moveTiro timelapse tiro = tiro
  {
    pos = (x + timelapse * vx, y + timelapse * vy)
  }
  where ( x,  y) = pos tiro
        (vx, vy) = vel tiro

-- | Spawna tiros
spawnTiro :: TVar Bool -> TVar Int -> IO ()
spawnTiro spawn dific = forever $ do
  dif <- readTVarIO dific
  let continha = 4000 - (1000*dif)
  let resposta = if (continha <= 500) then 500 else continha
  sleepMs resposta 
  atomically (writeTVar spawn True)
  where
    sleepMs n = threadDelay (n * 1000)

main = do
  ops <- getStdGen
  spawn' <- newTVarIO False
  dific' <- newTVarIO 1
  let maze = initialState ops dific' spawn'
  forkIO (spawnTiro (spawn maze) (dific maze))
  playIO window background fps maze render handleEvent update
