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

pentagenarian, glider, block, pulsar, blinker, diehard:: Grid
pentagenarian = [(1,2),(2,2),(2,3),(4,1),(4,3)]
glider = [(1,3),(2,1),(2,3),(3,2),(3,3)]
block = [(x,y) | x <- [1..3], y <- [1..3]]
pulsar = [(x+i,y) | x <- [2,8], y <- [0,5,7,12], i <- [0..2]] ++ [(x,y+i) | x <- [0,5,7,12], y <- [2,8], i <- [0..2]]
blinker = [(1,2),(2,2),(3,2)]
diehard = [(2,1),(2,2),(1,2),(6,1),(7,1),(8,1),(7,3)]

maxEpoch :: Int
maxEpoch = 100

translate :: (Int,Int) -> Grid -> Grid
translate (m,n) g = [(x + m, y + n) | (x, y) <- g]

isMortal :: Grid -> Bool
isMortal [] = True
isMortal g = isMortal(step g)

recur :: Grid -> Int -> Bool
recur g 0 = False
recur g n = case length g == 0 of
                            True -> True
                            False -> recur (step g) (n-1)

lifetime :: Grid -> Maybe Int
lifetime g = recur2 g maxEpoch

recur2 :: Grid -> Int -> Maybe Int
recur2 g 0 = Nothing
recur2 g n = case length g  == 0 of
                                True -> Just (maxEpoch - n)
                                False -> recur2 (step g) (n-1)

hasPeriod :: Grid -> Int -> Bool
hasPeriod g k = sameGrid g (last (take (k+1) (iterate step g)))

sameGrid :: Grid -> Grid -> Bool
sameGrid g1 g2 = sort g1 == sort g2

eventuallyPeriodic :: Grid -> Bool
eventuallyPeriodic g = recur1 g maxEpoch hasPeriod

findEventualPeriod :: Grid -> Maybe (Int,Int)
findEventualPeriod g = undefined

recur1 :: Grid -> Int -> (Grid -> Int -> Bool) -> Bool
recur1 g 0 b = False
recur1 g n b = case (hasPeriod g n) of
                    True -> True
                    False -> recur1 g (n-1) b
