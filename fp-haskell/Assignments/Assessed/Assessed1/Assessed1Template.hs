-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed1 (doubleList, firstDoubled , priceRange , allergyFree , checkSpec , checkSpec' , linearSort , counterexample , fromBin , toBin) where

import Data.List
import Data.Maybe

import Types

doubleList :: [a] -> [a]
doubleList = undefined

firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled = undefined

priceRange :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange = undefined

allergyFree :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree = undefined

sampletin :: Tin
sampletin = [[Nuts], [Dairy,Gluten], [], [Soy]]

checkSpec :: Spec -> Tin -> Bool
checkSpec = undefined

checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' = undefined

linearSort :: Ord a => [a] -> [a]
linearSort = undefined

counterexample :: [Int]
counterexample = undefined

fromBin :: Bin -> [Int]
fromBin = undefined

toBin :: [Int] -> Maybe Bin
toBin = undefined
