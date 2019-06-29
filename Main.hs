module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Control.Monad
import Prim

import Debug.Trace

width, height, offset, fps :: Int
width  = 800
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

mkStart :: Float -> Float -> Picture
mkStart x y = translate (x-1) y $ color green $ rectangleSolid 1 1

mkEnd :: Float -> Float -> Picture
mkEnd x y = translate (x+1) y $ color red $ rectangleSolid 1 1
  
mkWay :: Float -> Float -> Picture
mkWay x y = translate x y $ color wayColor $ rectangleSolid 1 1
  

-- | Guarda o estado do jogo.
data MazeState = Game
  { cells :: [(Int, Int)],
    cols  :: Int,
    rows  :: Int,
    began :: Bool}
  
-- | Renderiza o estado do jogo (converte pra Picture).
render :: MazeState -> Picture
render game =
  translate (-(fromIntegral width / 2.2)) (-(fromIntegral height / 2.2)) $
  scale ((fromIntegral width / (fromIntegral (cols game))) / 2.2)
  ((fromIntegral height / (fromIntegral (rows game))) / 2.2) $
  pictures $
  [
    mkWay (fromIntegral x) (fromIntegral y) | (x,y) <- cells game
  ] ++ [mkStart 0 0, mkEnd (2 * ((fromIntegral $ cols game) - 1)) (2 * ((fromIntegral $ rows game) - 1))]

-- | Converte de vertices e arestas para malha a ser pintada
graphToMaze :: Grafo -> [(Int, Int)]
graphToMaze g = [v | (v,_) <- g] ++
  [intermediarios | ((i,j), la) <- g,
    intermediarios <- [((x+i) `div` 2, (y+j) `div` 2) | (x,y) <- la]] ++
  [v | (_,la) <- g, v <- la]

-- | Inicializa o jogo com um estado.
initialState :: (RandomGen t) => t -> MazeState
initialState gen = Game
  { cells = graphToMaze $ gordifica $ generateMaze c r gen,
    cols = c,
    rows = r,
    began = False
  } where c = 40; r = 30
  
-- | Multiplica grafo por 2 para ficar bonitinho
gordifica :: Grafo -> Grafo
gordifica g = [((2*i,2*j),[(2*u, 2*v) | (u,v) <- la]) | ((i,j),la) <- g]

-- | Lida com eventos (n faz nada por enquanto)
-- esta funcao devera detectar que clicou no verde e iniciar o jogo
-- e tambem detectar movimento de mouse ahuehaue
handleEvent :: Event -> MazeState -> MazeState
handleEvent event game
  | EventKey (MouseButton LeftButton) Up _ pt@(x,y) <- event
  = trace ("handleEventKey " ++ show x ++ " " ++ show y) Game
    {
      cells = cells game,
      cols  = cols game,
      rows  = rows game,
      began = True
    }
  | EventMotion (x, y) <- event
  = trace ("handleEventMotion " ++ show x ++ " " ++ show y) Game
  {
    cells = cells game,
    cols  = cols game,
    rows  = rows game,
    began = began game
  }
  | otherwise = game

-- | Traz de coordenadas de camera pra coordenadas de labirinto
-- WIDTH
fromPixelW :: Float -> MazeState -> Int
fromPixelW w game = round ((w + iw / 2.2) / (iw/(fromIntegral $ rows game)/2.2))
  where iw = fromIntegral width

-- | Traz de coordenadas de camera pra coordenadas de labirinto
-- HEIGHT
fromPixelH :: Float -> MazeState -> Int
fromPixelH h game = round ((h + ih / 2.2) / (ih/(fromIntegral $ cols game)/2.2))
  where ih = fromIntegral height

-- | Atualiza mapa (n faz nada por enquanto)
-- possivelmente adicionar temporizador
update :: Float -> MazeState -> MazeState
update _ = teste

-- | Hmmm
teste :: MazeState -> MazeState
teste game = game

main = do
  ops <- getStdGen
  let maze = initialState ops
  play window background fps maze render handleEvent update