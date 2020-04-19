-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed3Solutions where

import Data.List
import Data.Tree

import Types
-- import DomViz  -- comment out as a last resort if you are unable to install diagrams

-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]

-- some example Domineering games
board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }

alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }

-- start of Question 1
--- Part 1
legalMoves :: Player -> Board -> [Cell]
legalMoves p b = [c | c <- free b, adjCell c p `elem` free b]

---- Using the higher-order function filter, see hoogle.haskell.org
legalMoves_1 :: Player -> Board -> [Cell]
legalMoves_1 p b = filter (\c -> adjCell c p `elem` free b) (free b)

---- Using some pattern matching to get 'free b' directly
legalMoves_2 :: Player -> Board -> [Cell]
legalMoves_2 p (Board _ f _) = filter (\c -> adjCell c p `elem` f) f

--- Part 2
moveLegal :: Board -> Cell -> Board
moveLegal (Board p f h) c = Board { turn = opp p,
                                    free = f \\ [c, adjCell c p],
                                    hist = c : h }

---- Using 'delete' twice
moveLegal_1 :: Board -> Cell -> Board
moveLegal_1 (Board p f h) c = Board { turn = (opp p),
                                      free = delete c (delete (adjCell c p) f),
                                      hist = c : h }

--- Part 3
---- Using both battern matching and 'b@' so as to retain the ability to refer
---- the whole board at once
replay :: Board -> [Board]
replay b@(Board _ _ [])    = [b]
replay b@(Board p f (c:h)) = replay b' ++ [b]
  where
    p' = opp p
    b' = Board { turn = p',
                 free = c : adjCell c p' : f,
                 hist = h }

replay_1 :: Board -> [Board]
replay_1 b = if null (hist b)
             then [b]
             else replay b' ++ [b]
  where
    p  = opp (turn b)
    c1 = head $ hist b
    c2 = adjCell c1 p
    b' = Board { turn = p,
                 free = c2 : c1 : free b,
                 hist = delete c1 (hist b) }

-- start of Question 2

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

--- Part 1
score :: Board -> Score
score b = if null (legalMoves p b)
          then Win (opp p)
          else Heu (numMoves V - numMoves H - sign p)
  where
    p = turn b
    sign :: Player -> Int
    sign H = -1
    sign V = 1
    numMoves :: Player -> Int
    numMoves p' = length (legalMoves p' b)

--- Part 2
minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax sfn (Node b ts) = if null ts
                          then Node (b, sfn b) []
                          else Node (b, extremum (turn b) ss) ts'
  where
    ts' = map (minimax sfn) ts
    ss  = [s | Node (_,s) _ <- ts']
    extremum :: Player -> [Score] -> Score
    extremum H = minimum
    extremum V = maximum

--- Part 3
bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves d sfn b = map (head . hist) bs
  where
    tree = prune d (gametree b)
    Node (_, best) ts = minimax sfn tree
    bs = [b' | Node (b', p) _ <- ts, p == best]

chooseSafe :: PickingMonad m => [a] -> m (Maybe a)
chooseSafe [] = return Nothing
chooseSafe xs = do
  i <- pick 0 (length xs - 1)
  return (Just (xs !! i))

randomBestPlay :: PickingMonad m => Int -> (Board -> Score)
               -> Board
               -> m (Maybe Cell)
randomBestPlay d sfn = chooseSafe . bestmoves d sfn

randomPlay :: PickingMonad m => Board -> m (Maybe Cell)
randomPlay b = chooseSafe (legalMoves (turn b) b)

-- start of Question 3

runGame :: PickingMonad m => (Board -> m (Maybe Cell))
        -> (Board -> m (Maybe Cell))
        -> Board
        -> m Board
runGame playH playV b@(Board p _ _) = do
  mc <- if (p == H)
        then playH b
        else playV b
  case mc of
    Nothing -> return b
    Just c  -> if valid b c
               then runGame playH playV (moveLegal b c)
               else return b

-- start of Question 4

carpets :: [Board]
carpets = [helper n | n <- [0,1..]]
  where
    helper :: Int -> Board
    helper 0 = board 1 1
    helper n = Board (opp p) f' []
      where
        Board p f _ = helper (n-1)
        f' = -- bottom row
             [(k*offset + x , y) | (x,y) <- f, k <- [0,1,2]] ++
             -- middle row, skipping the centre
             [(k*offset + x , offset + y) | (x,y) <- f, k <- [0,2]] ++
             -- top row
             [(k*offset + x , 2*offset + y) | (x,y) <- f, k <- [0,1,2]]
          where
            offset = 3^(n-1)

--- Using 'iterate', calculating the necessary shift from the board
carpets_1 :: [Board]
carpets_1 = iterate step initial
  where
    initial = board 1 1
    step :: Board -> Board
    step (Board p cells _) = Board { turn = opp p, free = cells', hist = [] }
      where
        n = maximum (map fst cells)
        cells' = cells ++
                 shift cells (n, 0) ++
                 shift cells (0, n) ++
                 shift cells (2 * n, 0) ++
                 shift cells (0, 2 * n) ++
                 shift cells (2 * n, n) ++
                 shift cells (n, 2 * n) ++
                 shift cells (2 * n, 2 * n)
        shift :: [Cell] -> Cell -> [Cell]
        shift cs (dx, dy) = [(x + dx, y + dy) | (x, y) <- cs]

-- Generate an empty board of the right size and decide if each cell should be filled
-- using an algorithm from Wikipedia (https://en.wikipedia.org/wiki/Sierpinski_carpet).
carpets_2 :: [Board]
carpets_2 = [helper n | n <- [0,1..]]
  where
    helper :: Int -> Board
    helper n = Board { turn = if n `mod` 2 == 0 then H else V
                     , free = filter isSierpinskiCarpetFilled [(x, y) | x <- [0..3^n - 1], y <- [0..3^n - 1]]
                     , hist = []
                     }
    isSierpinskiCarpetFilled :: Cell -> Bool
    isSierpinskiCarpetFilled (0, _) = True
    isSierpinskiCarpetFilled (_, 0) = True
    isSierpinskiCarpetFilled (x, y) = if x `mod` 3 == 1 && y `mod` 3 == 1 then False
                                      else isSierpinskiCarpetFilled (x `div` 3, y `div` 3)
