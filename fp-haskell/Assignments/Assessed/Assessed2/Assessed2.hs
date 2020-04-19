-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed2 (choose , simulate , cut , shuffle , riffles , permute , genTree) where

import Types
import Data.List

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

instance Show Rank where
  show RA = "A"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"
  show R9 = "9"
  show R10 = "10"
  show RJ = "J"
  show RQ = "Q"
  show RK = "K"

instance Show Suit where
  show C = "\9827"
  show D = "\9830"
  show H = "\9829"
  show S = "\9824"

red, black :: String -> String
red   s = "\x1b[31m" ++ s ++ "\x1b[0m"
black s = "\x1b[30m" ++ s ++ "\x1b[0m"

instance Show Card where
  show (Card r S) = (black (show r ++ show S))
  show (Card r C) = (black (show r ++ show C))
  show (Card r D) = (red (show r ++ show D))
  show (Card r H) = (red (show r ++ show H))

choose :: PickingMonad m => [a] -> m a
choose [x] = pure x
choose xs = do
            i <- pick 0 ((length xs)-1)
            e <- choose ([xs !! i])
            return e

simulate :: Monad m => m Bool -> Integer -> m Integer
simulate _ 0 = pure 0
simulate bm n = do
                b <- bm
                count <- simulate bm (n-1)
                if b == True
                then return (count + 1)
                else return count

cut :: PickingMonad m => [a] -> m ([a],[a])
cut [] = pure ([],[])
cut xs = do
        i <- pick 0 (length xs)
        return (splitAt i xs)

shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle ([],[]) = pure []
shuffle (ls,[]) = pure ls
shuffle ([],rs) = pure rs
shuffle (ls, rs) = do
                    poss <- pick 1 (length ls + length rs)
                    if poss <= length ls
                    then
                        do
                        rest <- shuffle (tail ls, rs)
                        return ((head ls):rest)
                    else
                        do
                        rest <- shuffle (ls, tail rs)
                        return ((head rs):rest)


riffles :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles _ _ 0 xs = pure xs
riffles cf sf n xs = do
                    divided <- cut xs
                    shuffled <- shuffle divided
                    riffles cf sf (n-1) shuffled

permute :: PickingMonad m => [a] -> m [a]
permute [] = pure []
permute (x:xs) = do
                i <- pick 0 (length xs)
                arr <- permute xs
                ins x arr i
                where
                ins :: PickingMonad m => a -> [a] -> Int -> m [a]
                ins v [] _ = pure [v]
                ins v xs i = return (fst (splitAt i xs) ++ [v] ++ snd (splitAt i xs))


genTree :: PickingMonad m => [a] -> m (Bin a)
genTree [x] = pure (L x)
genTree xs = do
                perm <- permute xs
                rantree perm


rantree :: PickingMonad m => [a] -> m (Bin a)
rantree [x] = pure (L x)
rantree xs = do
                i <- pick 1 (length xs-1)
                let left = fst (splitAt i xs)
                let right = snd (splitAt i xs)
                ltree <- rantree left
                rtree <- rantree right
                return (B ltree rtree)
