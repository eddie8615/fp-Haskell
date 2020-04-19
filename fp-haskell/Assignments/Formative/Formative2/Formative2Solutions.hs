-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Formative2Solutions (texample , tskinny , texpr , canopy , treePreOrder , eval , qsort) where

import System.Random
import Control.Monad.Identity
import Data.List -- for delete operation on lists

import Types

texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

tskinny = Node 1 [Node 2 [Node 3 [Node 4 [Leaf 'a']]]]

-- Question 1
canopy :: Tree a b -> [a]
canopy (Leaf x)    = [x]
canopy (Node _ ts) = concatMap canopy ts
--- concatMap is the same as concat . map

-- Question 2
treePreOrder :: Tree a b -> [Either a b]
treePreOrder (Leaf x)    = [Left x]
treePreOrder (Node y ts) = (Right y) : (concatMap treePreOrder ts)

texpr = Node Add [Node Mul [Leaf 1, Leaf 2], Node Mul [Leaf 3, Leaf 4, Leaf 5], Node Mul []]

-- Question 3
eval :: Num a => OpTree a -> a
eval (Leaf n)      = n
eval (Node Add ts) = sum (map eval ts)
eval (Node Mul ts) = product (map eval ts)

-- Question 4
qsort :: (Ord a, PickingMonad m) => [a] -> m [a]
qsort [] = return []
qsort xs = do
  pivotIndex <- pick 0 (length xs - 1)
  let pivot = xs !! pivotIndex
  let ys    = delete pivot xs
  smaller <- qsort [l | l <- ys, l <  pivot]
  bigger  <- qsort [r | r <- ys, r >= pivot]
  return (smaller ++ [pivot] ++ bigger)
