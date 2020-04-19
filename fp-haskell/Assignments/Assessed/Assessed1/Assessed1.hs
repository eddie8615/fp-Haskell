-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed1 (doubleList, firstDoubled , priceRange , allergyFree , checkSpec , checkSpec' , linearSort , counterexample , fromBin , toBin) where

import Data.List
import Data.Maybe

import Types

type Stack a = [a]

push :: a -> Stack a -> Stack a
push x [] = [x]
push x xs = x:xs

pop :: Stack a -> Stack a
pop [] = []
pop (x:xs) = xs

data LabBin a = Leaf | Branch a (LabBin a) (LabBin a) deriving (Show, Ord, Eq)

doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x:xs) = [x] ++ [x] ++ (doubleList xs)

firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled (x:xs)
            | xs == [] = Nothing
            | x == head(xs) = Just x
            | otherwise = firstDoubled xs

priceRange :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange (P _) (P _) [] = []
priceRange (P min) (P max) (cc:ccs)
                                | (getPrice cc) >= (P min) && (getPrice cc) <= (P max) = [cc] ++ priceRange (P min) (P max) ccs
                                | otherwise = priceRange (P min) (P max) ccs

getPrice :: Cupcake -> Price
getPrice (CC p _) = p

allergyFree :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree _ [] = []
allergyFree [] ccs = ccs
allergyFree ins (cc:ccs) = case checkContain ins (getRecipe cc) of
                            False -> [cc]++(allergyFree ins ccs)
                            True -> allergyFree ins ccs

checkContain :: [Ingredient] -> Recipe -> Bool
checkContain _ [] = False
checkContain ins (r:rs)
                    | r `elem` ins = True
                    | otherwise = checkContain ins rs

getRecipe :: Cupcake -> Recipe
getRecipe (CC _ []) = []
getRecipe (CC _ r) = r

sampletin :: Tin
sampletin = [[Nuts], [Dairy,Gluten], [], [Soy]]

checkSpec :: Spec -> Tin -> Bool
checkSpec sp tin = case sp of
                (HasCup num ing) -> case num < length(tin) of
                                    True -> ing `elem` (tin !! num)
                                    False -> False
                (And l r) -> (checkSpec l tin) && (checkSpec r tin)
                (Or l r) -> (checkSpec l tin) || (checkSpec r tin)
                (Not l) -> not(checkSpec l tin)

checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' sp tin = case (checkSpec sp tin) of
                    True -> Just True
                    False -> case (outOfRange sp (length tin)) of
                            True -> Nothing
                            False -> Just False

outOfRange :: Spec -> Int -> Bool
outOfRange sp 0 = True
outOfRange sp num = case sp of
                    (HasCup num' _) -> num' >= num
                    (And l r) -> (outOfRange l num) || (outOfRange r num)
                    (Or l r) -> (outOfRange l num) || (outOfRange r num)
                    (Not l) -> (outOfRange l num)

linearSort :: Ord a => [a] -> [a]
linearSort [] = []
linearSort xs = reverse (sorting xs [] [])

sorting :: Ord a => [a] -> Stack a -> [a] -> [a]
sorting xs ss op
                    | ss == [] && op == [] && xs == [] = []
                    | (ss == [] && op == []) || (not(xs == []) && ss == []) = sorting (pop xs) (push (head xs) ss) op
                    | ss == [] && xs == [] = op
                    | xs == [] = sorting xs (pop ss) ((head ss):op)
                    | otherwise = case  (head xs) <= (head ss) of
                                    True -> sorting (tail xs) (push (head xs) ss) op
                                    False -> sorting xs (pop ss) ((head ss):op)

counterexample :: [Int]
counterexample = [2,3,1]

fromBin :: Bin -> [Int]
fromBin L = []
fromBin bin = genList bin [1..countNode bin]

countNode :: Bin -> Int
countNode L = 0
countNode (B l r) = 1 + countNode l + countNode r

genList :: Bin -> [Int] -> [Int]
genList L xs = xs
genList (B l r) xs = [x] ++ genList l (delete x (getL (splitAt i xs))) ++ genList r (getR (splitAt i xs))
                        where x = getElem xs i
                              i = countNode (B l r) - countNode r

getElem :: [Int] -> Int -> Int
getElem xs i = xs !! (i - 1)

getL :: ([Int], [Int]) -> [Int]
getL (a,b) = a

getR :: ([Int], [Int]) -> [Int]
getR (a,b) = b

toBin :: [Int] -> Maybe Bin
toBin [] = Nothing
toBin xs = case (linearSort xs) == [1..(length xs)] of
                True -> Just (binConverter (genBin xs Leaf))
                False -> Nothing

genBin :: [Int] -> LabBin Int -> LabBin Int
genBin [] t = t
genBin (x:xs) Leaf = genBin xs (Branch x Leaf Leaf)
genBin (x:xs) t = genBin xs (insertNode x t)

insertNode :: Int -> LabBin Int -> LabBin Int
insertNode x Leaf = Branch x Leaf Leaf
insertNode x (Branch a l r)
                        | x < a = Branch a (insertNode x l) r
                        | otherwise = Branch a l (insertNode x r)

binConverter :: LabBin a -> Bin
binConverter Leaf = L
binConverter (Branch a l r) = B (binConverter l) (binConverter r)
