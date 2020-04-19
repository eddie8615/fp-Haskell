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
  show = undefined

instance Show Suit where
  show = undefined

red, black :: String -> String
red   s = "\x1b[31m" ++ s ++ "\x1b[0m"
black s = "\x1b[30m" ++ s ++ "\x1b[0m"

instance Show Card where
  show = undefined

choose :: PickingMonad m => [a] -> m a
choose = undefined

simulate :: Monad m => m Bool -> Integer -> m Integer
simulate = undefined

cut :: PickingMonad m => [a] -> m ([a],[a])
cut = undefined

shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle = undefined

riffles :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles = undefined

permute :: PickingMonad m => [a] -> m [a]
permute = undefined

genTree :: PickingMonad m => [a] -> m (Bin a)
genTree = undefined
