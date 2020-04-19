-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Formative2 (texample , tskinny , texpr , canopy , treePreOrder , eval , qsort) where

import System.Random
import Control.Monad.Identity

import Types

texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

tskinny = Node 1 [Node 2 [Node 3 [Node 4 [Leaf 'a']]]]

canopy :: Tree a b -> [a]
canopy = undefined

treePreOrder :: Tree a b -> [Either a b]
treePreOrder = undefined

texpr = Node Add [Node Mul [Leaf 1, Leaf 2], Node Mul [Leaf 3, Leaf 4, Leaf 5], Node Mul []]

eval :: Num a => OpTree a -> a
eval = undefined

qsort :: (Ord a, PickingMonad m) => [a] -> m [a]
qsort = undefined

instance PickingMonad IO where
  pick lo hi = getStdRandom (randomR (lo, hi))

instance PickingMonad Identity where
  pick lo hi = Identity lo

