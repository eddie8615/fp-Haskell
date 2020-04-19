{-# LANGUAGE Trustworthy #-}

module DomViz (boardSVG, boardSVG', gametreeSVG, gametreeSVG') where

import Data.Maybe

import Diagrams.Prelude hiding (turn)
import Diagrams.Backend.SVG
import Diagrams.TwoD.Layout.Tree

import Types

import Data.Tree

adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

opp :: Player -> Player
opp H = V
opp V = H

cells :: Board -> [Cell]
cells b = free b ++
          concatMap (\(p,c) -> [c,adjCell c p])
                    (zip (iterate opp (opp (turn b))) (hist b))

domino :: Int -> Player -> Diagram B
domino i H = translateX 0.5
                (text (show i) # font "sansserif" # fc white # scale 0.6 # translateX 0.1 # translateY (-0.2) `atop`
                 square 1 # scaleX 2 # scale 0.9 # fc darkred # lw none)
domino i V = translateY 0.5
                (text (show i) # font "sansserif" # fc black # scale 0.6 # translateX 0.1 # translateY (-0.2) `atop`
                 square 1 # scaleY 2 # scale 0.9 # fc lightblue # lw none)

cell :: Diagram B
cell = (square 0.95 # fc lightyellow # lw none) `atop` (square 1 # fc darkgray # lw none)


boardDiagram :: Board -> Diagram B
boardDiagram b =
  (position [(p2 (fromIntegral x,fromIntegral y), domino i p) |
             (i,p,(x,y)) <- zip3 [1..] (iterate opp startP) (reverse ((maxX+dX,maxY+dY):hist b))]
            `atop`
   position [(p2 (fromIntegral x,fromIntegral y), cell) | (x,y) <- cells b])
  # centerXY # bg slategray
  where
    startP = if even (length (hist b)) then turn b else opp (turn b)
    maxX = maximum (map fst (cells b))
    maxY = maximum (map snd (cells b))
    (dX,dY) = if turn b == H then (1,2) else (2,1)

boardSVG' :: Double -> Board -> String -> IO ()
boardSVG' scale b basename =
  renderPretty (basename ++ ".svg") (mkWidth width) (boardDiagram b)
  where
    xs = map fst (cells b)
    width = scale * 40 * fromIntegral (maximum xs - minimum xs + 2)

boardSVG :: Board -> String -> IO ()
boardSVG = boardSVG' 1

gametreeDiagram :: Tree (Board,String) -> Diagram B
gametreeDiagram t =
  renderTree toDiagram (~~)
  (symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX . toDiagram
                     & slHeight .~ fromMaybe (0,0) . extentY . toDiagram)
   t)
  where
    toDiagram :: (Board,String) -> Diagram B
    toDiagram (b,s) = (alignTR (topLeftText s # font "sansserif" # fc white # scale 0.6) `atop` alignTL (boardDiagram b)) # centerXY # scale 0.2

gametreeSVG' :: Double -> Tree (Board,String) -> String -> IO ()
gametreeSVG' scale t basename = do
  renderPretty (basename ++ ".svg") (mkWidth 1024) (gametreeDiagram t # centerXY # pad 1.1 # bg lightgray)
  where
    width = scale * 40 * fromIntegral (nodes t)
    nodes (Node _ ts) = 1 + sum (map nodes ts)

gametreeSVG :: Tree Board -> String -> IO ()
gametreeSVG = gametreeSVG' 1 . fmap (\b -> (b,""))
