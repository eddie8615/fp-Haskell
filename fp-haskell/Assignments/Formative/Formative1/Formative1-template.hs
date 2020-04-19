module Formative1 (translate, isMortal , lifetime , hasPeriod , findEventualPeriod , eventuallyPeriodic) where
import Data.List

type Cell = (Int,Int)
type Grid = [Cell]

isLive, isDead :: Cell -> Grid -> Bool
isLive c g = c `elem` g
isDead c g = not (isLive c g)

neighbours :: Cell -> [Cell]
neighbours (x,y) = [ (x+i,y+j) | i <- [-1..1], j <- [-1..1], not (i==0 && j==0) ]

liveNeighbours :: Grid -> Cell -> [Cell]
liveNeighbours g c = [c' | c' <- neighbours c, isLive c' g]

step :: Grid -> Grid
step [] = []
step g =
  [(x,y) | x <- [minX-1 .. maxX+1],
           y <- [minY-1 .. maxY+1],
              (isDead (x,y) g && length (liveNeighbours g (x,y)) == 3)
           || (isLive (x,y) g && length (liveNeighbours g (x,y)) `elem` [2,3])
         ]
  where
    minX = minimum [ x | (x,y) <- g ]
    maxX = maximum [ x | (x,y) <- g ]
    minY = minimum [ y | (x,y) <- g ]
    maxY = maximum [ y | (x,y) <- g ]

pentagenarian, glider, block, pulsar :: Grid
pentagenarian = [(1,2),(2,2),(2,3),(4,1),(4,3)]
glider = [(1,3),(2,1),(2,3),(3,2),(3,3)]
block = [(x,y) | x <- [1..3], y <- [1..3]]
pulsar = [(x+i,y) | x <- [2,8], y <- [0,5,7,12], i <- [0..2]] ++ [(x,y+i) | x <- [0,5,7,12], y <- [2,8], i <- [0..2]]

translate :: (Int,Int) -> Grid -> Grid
translate (m,n) g = undefined

isMortal :: Grid -> Bool
isMortal g = undefined

lifetime :: Grid -> Maybe Int
lifetime g = undefined

hasPeriod :: Grid -> Int -> Bool
hasPeriod g k = undefined

sameGrid :: Grid -> Grid -> Bool
sameGrid g1 g2 = sort g1 == sort g2

eventuallyPeriodic :: Grid -> Bool
eventuallyPeriodic g = undefined

findEventualPeriod :: Grid -> Maybe (Int,Int)
findEventualPeriod g = undefined

