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
canopy (Leaf a) = [a]
canopy (Node b []) = []
canopy (Node b (t:ts)) = canopy t ++ canopy (Node b ts)

treePreOrder :: Tree a b -> [Either a b]
treePreOrder (Leaf a) = [Left a]
treePreOrder (Node b []) = [Right b]
treePreOrder (Node b ts) = [Right b] ++ concat (map (treePreOrder) ts)

texpr = Node Add [Node Mul [Leaf 1, Leaf 2], Node Mul [Leaf 3, Leaf 4, Leaf 5], Node Mul []]

eval :: Num a => OpTree a -> a
eval (Leaf a) = a
eval (Node op []) = 1
eval (Node op (t:ts)) = case op of
                    Mul -> foldl (*) (eval t) (map eval ts)
                    Add -> foldl (+) (eval t) (map eval ts)

qsort :: (Ord a, PickingMonad m) => [a] -> m [a]
qsort [] = pure []
qsort [x] = pure [x]
qsort xs = do
        p <- pivot
        x <- qsort (filter (< (xs !! p)) (delete (xs !! p) xs))
        y <- qsort (filter (>= (xs !! p)) (delete (xs !! p) xs))
        return (x ++ [xs !! p] ++ y) where pivot = pick 0 ((length xs)-1)

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete v (x:xs)
                | v == x = xs
                | otherwise = x : delete v xs
