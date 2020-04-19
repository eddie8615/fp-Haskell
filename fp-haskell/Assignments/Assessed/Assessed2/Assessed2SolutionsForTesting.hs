-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed2SolutionsForTesting (choose , simulate , cut , shuffle , riffles , permute , genTree , 
                                      _show_rank , _show_suit, _show_suit' , _show_card ,
                                      normalise) where

import Types
import Data.List

-- Extra import
import Control.Monad

standard52 :: Deck
standard52 = [Card {rank = r, suit = s} | r <- [R2 .. RA], s <- [C .. S]]

code :: PickingMonad m => m Char
code = do
  i <- pick 0 3
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalise :: Eq a => Dist a -> Dist a
normalise xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub" removes duplicates from a list


-- Question 1
--- Part 1
_show_rank R2  = "2"
_show_rank R3  = "3"
_show_rank R4  = "4"
_show_rank R5  = "5"
_show_rank R6  = "6"
_show_rank R7  = "7"
_show_rank R8  = "8"
_show_rank R9  = "9"
_show_rank R10 = "10"
_show_rank RJ  = "J"
_show_rank RQ  = "Q"
_show_rank RK  = "K"
_show_rank RA  = "A"

---- Using fromEnum
-- instance Show Rank where
--   show RA = "A"
--   show RK = "K"
--   show RQ = "Q"
--   show RJ = "J"
--   show x  = show . (+2) . fromEnum $ x

--- Part 2
---- Using unicode characters
_show_suit :: Suit -> String 
_show_suit C = "♣"
_show_suit D = "♦"
_show_suit H = "♥"
_show_suit S = "♠"

_show_suit' :: Suit -> String 
_show_suit' C = "9827"
_show_suit' D = "9830"
_show_suit' H = "9829"
_show_suit' S = "9824"

---- Using escape codes
-- instance Show Suit where
--   show C = "\9827"
--   show D = "\9830"
--   show H = "\9829"
--   show S = "\9824"

red, black :: String -> String
red   s = "\x1b[31m" ++ s ++ "\x1b[0m"
black s = "\x1b[30m" ++ s ++ "\x1b[0m"

--- Part 3
---- Using pattern matching
{-
instance Show Card where
  show (Card rank suit) = let text = show rank ++ show suit in case suit of
    D -> red text
    H -> red text
    _ -> black text
-}

_show_card :: Card -> String 
_show_card (Card rank suit) = let text = _show_rank rank ++ _show_suit suit in case suit of
    D -> red text
    H -> red text
    _ -> black text

---- Using 'rank' and 'suit'
-- instance Show Card where
--   show card = let (r,s) = (rank card,suit card) in
--               if s `elem` [D,H]
--               then red (show r ++ show s)
--               else black (show r ++ show s)


-- Question 2
--- Part 1
---- Using do notation
choose :: PickingMonad m => [a] -> m a
choose xs = do
  i <- pick 0 (length xs - 1)
  return (xs !! i)

---- Using '<$>', provide error message for empty list
choose_1 :: PickingMonad m => [a] -> m a
choose_1 [] = error "Can't choose element from an empty list"
choose_1 xs = (!!) xs <$> pick 0 (length xs - 1)

--- Part 2
---- Using do notation
simulate :: Monad m => m Bool -> Integer -> m Integer
simulate bm 0 = return 0
simulate bm n = do
  k <- simulate bm (n-1)
  b <- bm
  if b
  then return (k+1)
  else return k

---- Using '>>=' and '<$>', provide error message for n < 0
simulate_1 :: Monad m => m Bool -> Integer -> m Integer
simulate_1 bm n
  | n < 0 = error "Cannot simulate a negative number of time"
  | n == 0 = return 0
  | otherwise = bm >>= \trial -> (if trial then (+1) else id) <$> simulate_1 bm (n-1)

---- Using replicateM
simulate_2 :: Monad m => m Bool -> Integer -> m Integer
simulate_2 bm n = genericLength . filter id <$> replicateM (fromIntegral n) bm

-- Question 3
--- Part 1
cut :: PickingMonad m => [a] -> m ([a],[a])
cut xs = do
  i <- pick 0 (length xs)
  return (splitAt i xs)

--- Part 2
---- Using nested 'do'
shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle (xs,[])     = return xs
shuffle ([],ys)     = return ys
shuffle (x:xs,y:ys) = do
  let m = length (x:xs)
  let n = length (y:ys)
  k <- pick 1 (m + n) 
  if k <= m
  then do
    zs <- shuffle (xs,y:ys)
    return (x:zs)
  else do
    zs <- shuffle (x:xs,ys)
    return (y:zs)

---- Using '<$>'
shuffle_1 :: PickingMonad m => ([a],[a]) -> m [a]
shuffle_1 ([], ys) = return ys
shuffle_1 (xs, []) = return xs
shuffle_1 (x:xs, y:ys) = do
  let m = length (x:xs)
  let n = length (y:ys)
  k <- pick 1 (m + n)
  if k <= m
    then (x:) <$> shuffle_1 (xs,y:ys)
    else (y:) <$> shuffle_1 (x:xs,ys)

--- Part 3
riffles :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles cf sf n xs
  | n < 0 = error "Can't perform a negative number of shuffles"
  | n == 0 = return xs
  | otherwise = do
      cutDeck <- cf xs
      shuffledDeck <- sf cutDeck
      riffles cf sf (n-1) shuffledDeck

---- Doing the recursive part first
riffles_1 :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles_1 cf sf 0 xs = return xs
riffles_1 cf sf n xs = do
  xs' <- riffles cf sf (n-1) xs
  (ys,zs) <- cf xs'
  sf (ys,zs)

---- Using higher order functions
riffles_2 :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles_2 cf sf n xs = foldM (\ys _ -> cf ys >>= sf) xs [1..n]

-- Question 4
--- Part 1
permute :: PickingMonad m => [a] -> m [a]
permute [] = return []
permute (x:xs) = do
  ys <- permute xs
  insertionPoint <- pick 0 (length ys)
  return (insertAt insertionPoint x ys)
    where
      insertAt 0 x ys     = x : ys
      insertAt n x []     = error "Inserted past end of list"
      insertAt n x (y:ys) = y: insertAt (n-1) x ys
---- Can also have (less efficiently)
---- 'insertAt n x ys = take n ys ++ [x] ++ drop n ys'

--- Part 2
---- Helper function to get number of subtrees of a tree
size :: Bin a -> Int
size (L _)   = 1
size (B l r) = 1 + size l + size r

genTree :: PickingMonad m => [a] -> m (Bin a)
genTree [] = error "Empty list"
genTree [x] = return (L x)
genTree (x:xs) = do
  -- Apply genTree to tail
  t <- genTree xs
  -- Pick a number between 1 and number of subtrees
  n <- pick 1 (size t)
  -- Flip a coin to insert on left or right
  goLeft <- choose [True,False]
  -- Insert the node into the nth subtree
  return $ insertIntoTree x n goLeft t
    where
      insertIntoTree :: a -> Int -> Bool -> Bin a -> Bin a
      insertIntoTree y n left (L z) = if left
                                      then B (L y) (L z)
                                      else B (L z) (L y)
      insertIntoTree y n left (B l r)
        | n <= s = B (insertIntoTree y n left l) r
        | n == s + 1 = if left
                       then B (L y) (B l r)
                       else B (B l r) (L y)
        | otherwise = B l (insertIntoTree y (n - s - 1) left r)
        where s = size l
