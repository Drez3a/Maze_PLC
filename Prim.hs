module Prim where

import System.Random
import Control.Monad

type Vertice = (Int, Int)
type ListaAdj = (Vertice, [Vertice])

type Grafo = [ListaAdj]

inicializaGrafoConexo :: Int -> Int -> Grafo
inicializaGrafoConexo width height = listaAdj
  where
    vertices  = [(i, j) | i <- [0..width-1], j <- [0..height-1]]
    listaAdj  = [((i,j), verticesAdjacentes (i,j) width height) | (i, j) <- vertices]

verticesAdjacentes vertice width height = map (\((i,x),(j,y)) -> (i+x, j+y)) $ filter (\((i,x),(j,y)) -> if (x+i >= width || y+j >= height || x+i < 0 || y+j < 0 || (x==y) || x+y == 0) then False else True) [((i,x), (j,y)) | let (i, j) = vertice, x <- [(-1), 0, 1], y <- [(-1), 0, 1]]

getLista :: Vertice -> Grafo -> ListaAdj
getLista v [] = error "n encontrado"
getLista v ((v1, la):xs)
  | v == v1 = (v1,la)
  | otherwise = getLista v xs

generateMaze :: (RandomGen t) => Int -> Int -> t -> Grafo
generateMaze width height gen = randPrim g f [v] [(v,q)] gen
  where
    (v,q) = g !! 0   -- escolhe celula e adiciona paredes
    f     = [(v,[])] -- marca como parte do labirinto
    g     = inicializaGrafoConexo width height

randPrim :: (RandomGen t) => Grafo -> Grafo -> [Vertice] -> Grafo -> t -> Grafo
randPrim _ f _ [] gen = f
randPrim g f v q  gen = randPrim g novoF novoV novoQ novoGen
  where
    (v', la) = q !! pos
    parede = la !! pos'
    visitado = outroVisitado parede v
    (pos, novoGen') = randomR (0, length q - 1) gen
    (pos', novoGen) = randomR (0, length la - 1) novoGen'
    novoF' = if (not visitado) then adicionaVertice v' f else f
    novoF  = if (not visitado) then adicionaPassagem v' parede novoF' else novoF'
    novoV  = if (not visitado) then v ++ [parede] else v
    novoQ' = if (not visitado) then q ++ [getLista parede g] else q
    novoQ  = removeParede v' parede novoQ'

removeParede :: Vertice -> Vertice -> Grafo -> Grafo
removeParede v1 v2 ((v, la):xs)
  | v == v1 = x
  | otherwise = (v,la):(removeParede v1 v2 xs)
  where
    (x', la') = (v,[x | x<-la, x /= v2])
    x  = if (null la') then xs else (x', la'):xs
    
    

adicionaVertice :: Vertice -> Grafo -> Grafo
adicionaVertice v g
  | possuiVertice v g = g
  | otherwise = g ++ [(v,[])]

possuiVertice :: Vertice -> Grafo -> Bool
possuiVertice v [] = False
possuiVertice v ((v',la):xs)
  | v == v' = True
  | otherwise = possuiVertice v xs

adicionaPassagem :: Vertice -> Vertice -> Grafo -> Grafo
adicionaPassagem v1 v2 ((v, la):xs)
  | v == v1 = (v,la++[v2]):xs
  | otherwise = (v,la):(adicionaPassagem v1 v2 xs)

outroVisitado :: Vertice -> [Vertice] -> Bool
outroVisitado v [] = False
outroVisitado v (x:xs)
  | v == x = True
  | otherwise = outroVisitado v xs
