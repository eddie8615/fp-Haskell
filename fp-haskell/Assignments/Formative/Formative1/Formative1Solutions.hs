module Formative1Solutions
  (translate, isMortal , lifetime , hasPeriod ,
   findEventualPeriod , eventuallyPeriodic) where

import Data.List
import Data.Maybe

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

{- There are multiple solutions to each question. The solutions showcase
   different approaches and Haskell techniques. They generally go from more
   basic to quite advanced. -}

-- Question 1
translate :: (Int,Int) -> Grid -> Grid
translate (m,n) g = [(x+m,y+n) | (x,y) <- g]

--- Using pattern matching
translate_1 :: (Int,Int) -> Grid -> Grid
translate_1 (m,n) []           = []
translate_1 (m,n) ((x,y):rest) = (x+m,y+n) : translate_1 (m,n) rest

--- Using map
translate_2 :: (Int,Int) -> Grid -> Grid
translate_2 (m,n) g = map (\(x,y) -> (x+m,y+n)) g

--- Using map, but "pointfree"
--- https://wiki.haskell.org/Pointfree
translate_3 :: (Int,Int) -> Grid -> Grid
translate_3 (m,n) = map (\(x,y) -> (x+m,y+n))

-- Question 2a
isMortal :: Grid -> Bool
isMortal [] = True
isMortal g  = isMortal (step g)

--- Using if then else
isMortal_1 :: Grid -> Bool
isMortal_1 g = if g == []
               then True
               else isMortal_1 (step g)

--- Using the function null from the standard library
isMortal_2 :: Grid -> Bool
isMortal_2 g = if null g
               then True
               else isMortal_2 (step g)

--- Using guards
isMortal_3 :: Grid -> Bool
isMortal_3 g
  | g == []   = True
  | otherwise = isMortal_3 (step g)

--- Using lifetime (Question 2b)
isMortal_4 :: Grid -> Bool
isMortal_4 g = isJust (lifetime g)

{- The versions of isMortal above will loop forever if a grid is not mortal. In
   some cases, we can detect such grids and output False. This is what the code
   below does. It keeps track of the grids and if a (non-empty) grid reappears
   (taking translations and rotations into account), then the grid is periodic
   (see Q3) and so will never die out.

   The code below is quite advanced, please don't feel bad if you don't
   understand it (yet)! -}

---- Helper functions
isTranslated :: Grid -> Grid -> Bool
isTranslated xs ys = foldr (\x b -> helper x ys || b) False xs
  where
    helper :: Cell -> Grid -> Bool
    helper (m,n) ((a,b) : ys) = let newys = translate (m-a, n-b) ((a,b) : ys) in
                                  sameGrid xs newys
    helper _ _ = False

rotate :: Grid -> Grid
rotate = map (\(x,y) -> (y, -x))

isRotated :: Grid -> Grid -> Bool
isRotated g g' =
  let
    g1 = rotate g
    g2 = rotate g1
    g3 = rotate g2
  in
  helper g' [g,g1,g2,g3]
  where
    helper :: Grid -> [Grid] -> Bool
    helper g gs = foldr (\x b -> g == x || b) False gs ||
                  foldr (\x b -> sameGrid g x || b) False gs ||
                  foldr (\x b -> isTranslated g x || b) False gs

hasSamePattern :: Grid -> Grid -> Bool
hasSamePattern g g' = sameGrid g g' || isTranslated g g' || isRotated g g'

isHistory :: [Grid] -> Grid -> Bool
isHistory hist g = foldr (\ x b -> hasSamePattern x g || b) False hist
---- End of helper functions

isMortal_5 :: Grid -> Bool
isMortal_5 = helper []
  where
    helper :: [Grid] -> Grid -> Bool
    helper hist g
      | g == []   = True
      | otherwise = if isHistory hist g
                    then False
                    else helper (g : hist) (step g)

-- Question 2b
lifetime :: Grid -> Maybe Int
lifetime [] = Just 0
lifetime g  = helper (lifetime (step g))
  where
    helper :: Maybe Int -> Maybe Int
    helper (Just n) = Just (n + 1)
    helper Nothing  = Nothing

--- Using that Maybe is a functor
--- (you will learn about this in a later lecture)
lifetime_1 :: Grid -> Maybe Int
lifetime_1 [] = Just 0
lifetime_1 g  = fmap (+1) (lifetime_1 (step g))

--- Using the clever helper functions from Question 2
lifetime_2 :: Grid -> Maybe Int
lifetime_2 = helper []
  where
    helper :: [Grid] -> Grid -> Maybe Int
    helper hist g
      | g == []   = Just (length hist)
      | otherwise = if isHistory hist g
                    then Nothing
                    else helper (g : hist) (step g)

-- Question 3
hasPeriod :: Grid -> Int -> Bool
hasPeriod g k = sameGrid (iterate step g !! k) g
--- Tip: use hoogle.haskell.org to find functions like iterate.

sameGrid :: Grid -> Grid -> Bool
sameGrid g1 g2 = sort g1 == sort g2

-- Question 4a
--- The idea is: create a list [g, step g, step(step g), ...] of grids. As soon
--- as a newly created grid already appears in the list, we know that the grid
--- is eventually periodic.
eventuallyPeriodic :: Grid -> Bool
eventuallyPeriodic g = helper [] g
  where
    helper :: [Grid] -> Grid -> Bool
    helper hist g' = if any (\x -> sameGrid g' x) hist
                     then True
                     else helper (hist ++ [g']) (step g')

--- Using findEventualPeriod from below
eventuallyPeriodic_1 :: Grid -> Bool
eventuallyPeriodic_1 g = isJust (findEventualPeriod g)

-- Question 4b
--- Roughly the same idea as for eventuallyPeriodic, but if a newly created grid
--- already appears in the list, then we can compute n as the position of the
--- grid in the list and k as the amount of steps it took to get the same grid
--- (length of the list - n).
findEventualPeriod :: Grid -> Maybe (Int,Int)
findEventualPeriod g = helper [] g
  where
    helper :: [Grid] -> Grid -> Maybe (Int,Int)
    helper hist g' =
      case findIndex (\x -> sameGrid g' x) hist of
        Just n  -> Just (n, length hist - n)
        Nothing -> helper (hist ++ [g']) (step g')

--- Using list comprehension and hasPeriod
findEventualPeriod_1 :: Grid -> Maybe (Int, Int)
findEventualPeriod_1 g = find isSolution solutionSpace
  where
    isSolution :: (Int,Int) -> Bool
    isSolution (n,k) = hasPeriod (iterate step g !! n) k
    solutionSpace = [(n,s-n) | s <- [1..], n <- [0..s - 1]]
    ---- note solutionSpace = [(n,k) | n <- [0..], k <- [1..]] doesn't work as
    ---- this list is [(0,1), (0,2), (0,3), ...]
