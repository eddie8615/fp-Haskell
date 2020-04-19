-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-# LANGUAGE Safe #-}

module Assessed1Solutions
  (doubleList, firstDoubled , priceRange , allergyFree , checkSpec ,
   checkSpec' , linearSort , counterexample, fromBin, toBin) where

import Types

import Data.List
import Data.Maybe

{- There are multiple solutions to each question. The solutions showcase
   different approaches and Haskell techniques. They generally go from more
   basic to quite advanced. -}

-- Question 1a
doubleList :: [a] -> [a]
doubleList []     = []
doubleList (x:xs) = x:x:doubleList xs

--- Using concatMap from the standard library (and pointfree)
doubleList_1 :: [a] -> [a]
doubleList_1 = concatMap (\x -> [x,x])
--- This is of course the same as doubleList_1 = concat . map (\x -> [x,x]).

--- Using foldr (and pointfree)
--- see /LectureNotes/data.md#hofns
doubleList_2 :: [a] -> [a]
doubleList_2 = foldr (\x y -> x:x:y) []

--- Using the list monad and do notation
doubleList_3 :: [a] -> [a]
doubleList_3 xs = do
  x <- xs
  [x,x]
  
-- Question 1b
firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled []       = Nothing
firstDoubled [x]      = Nothing
firstDoubled (x:y:ys) = if x == y
                        then Just x
                        else firstDoubled (y:ys)

-- Question 2a
priceRange :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange minPrice maxPrice cupcakes =
  filter (\(CC p _) -> minPrice <= p && p <= maxPrice) cupcakes
--- Note that you can pattern match within (\... -> ...).
--- Tip: use hoogle.haskell.org to find functions like filter.
--- Also see: /LectureNotes/data.md#hofns.

--- Pointfree
priceRange_1 :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange_1 minPrice maxPrice =
  filter (\(CC p _) -> minPrice <= p && p <= maxPrice)

--- Using list comprehension
priceRange_2 :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange_2 minPrice maxPrice cupcakes =
  [ c | c <- cupcakes, let CC p _ = c in minPrice <= p && p <= maxPrice ]

--- Using recursion (on lists)
priceRange_3 :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange_3 minPrice maxPrice [] = []
priceRange_3 minPrice maxPrice (CC p ingredients : cs) =
  if minPrice <= p && p <= maxPrice
  then CC p ingredients : priceRange_3 minPrice maxPrice cs
  else priceRange_3 minPrice maxPrice cs

-- Question 2b
allergyFree :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree allergens cupcakes = filter (not . helper) cupcakes
 where
   helper :: Cupcake -> Bool
   helper (CC _ ingredients) = any (\i -> i `elem` allergens) ingredients

--- Pointfree
allergyFree_1 :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree_1 allergens =
  filter (\(CC _ ingredients) -> not (any (`elem` allergens) ingredients))

--- Using list comprehension
allergyFree_2 :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree_2 allergens cupcakes =
  [ c | c <- cupcakes,
        let CC _ ingredients = c in not (any (`elem` allergens) ingredients)]

--- Using recursion (on lists)
allergyFree_3 :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree_3 allergens [] = []
allergyFree_3 allergens (CC p ingredients : cs) = 
  if any (`elem` ingredients) allergens
  then allergyFree_3 allergens cs
  else (CC p ingredients : allergyFree_3 allergens cs)

-- Question 3a
checkSpec :: Spec -> Tin -> Bool
checkSpec (And s1 s2) t  = checkSpec s1 t && checkSpec s2 t
checkSpec (Or s1 s2) t   = checkSpec s1 t || checkSpec s2 t
checkSpec (Not s) t      = not (checkSpec s t)
checkSpec (HasCup k i) t = i `elem` t !! k

-- Question 3b
checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' (And s1 s2) t  = case (checkSpec' s1 t,checkSpec' s2 t) of
                              (Nothing,_)       -> Nothing
                              (_,Nothing)       -> Nothing
                              (Just b1,Just b2) -> Just (b1 && b2)
checkSpec' (Or s1 s2) t   = case (checkSpec' s1 t,checkSpec' s2 t) of
                              (Nothing,_)       -> Nothing
                              (_,Nothing)       -> Nothing
                              (Just b1,Just b2) -> Just (b1 || b2)
checkSpec' (Not s) t      = case (checkSpec' s t) of
                              Nothing -> Nothing
                              Just b  -> Just (not b)
checkSpec' (HasCup k i) t = if 0 <= k && k < length t
                            then Just (checkSpec (HasCup k i) t)
                            else Nothing

--- Using do notation, which we will be taught in a later lecture on Monads
checkSpec'_1 :: Spec -> Tin -> Maybe Bool
checkSpec'_1 (And s1 s2) t = do
  b1 <- checkSpec'_1 s1 t
  b2 <- checkSpec'_1 s2 t
  return (b1 && b2)           
checkSpec'_1 (Or s1 s2) t  = do
  b1 <- checkSpec'_1 s1 t
  b2 <- checkSpec'_1 s2 t
  return (b1 || b2)
checkSpec'_1 (Not s) t     = do
  b <- checkSpec'_1 s t
  return (not b)
checkSpec'_1 (HasCup k i) t = if 0 <= k && k < length t
                              then Just (checkSpec (HasCup k i) t)
                              else Nothing

-- Question 4a
linearSort :: Ord a => [a] -> [a]
linearSort xs = helper [] xs
 where
   helper :: Ord a => [a] -> [a] -> [a]
   helper ys [] = ys
   helper [] (x:xs) = helper [x] xs
   helper (y:ys) (x:xs) = if x <= y
                          then helper (x:y:ys) xs
                          else y:helper ys (x:xs)

-- Question 4b
--- https://en.wikipedia.org/wiki/Stack-sortable_permutation#Sorting_with_a_stack
counterexample :: [Int]
counterexample = [2,3,1]

-- Question 5a (fromBin)
--- We first define some helper functions and explain the idea of the bijection.
noOfBs :: Bin -> Int
noOfBs L         = 0
noOfBs (B t1 t2) = noOfBs t1 + noOfBs t2 + 1

--- Decorated binary trees, taken from /LectureNotes/data.md#bintrees.
data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)

{- Decorate a Bin tree by labelling the nodes from left to right, e.g.
      *                 2
     / \               / \
    *   *     --->    1   3 
   / \ / \           / \ / \

   and

       *                 3
      / \               / \
     /   \             /   \
    *                 1
   / \        --->   / \
      *                 2
     / \               / \

   and

     *                 1
    / \               / \
   /   \             /   \
        *                 3
       / \    --->       / \
      *                 2
     / \               / \                                             -}
toBT :: Bin -> BT Int
toBT L         = Empty
toBT (B t1 t2) = Fork (n+1) (toBT t1) (shift (n+1) (toBT t2))
  where
    n = noOfBs t1
    shift :: Int -> BT Int -> BT Int
    shift m Empty = Empty
    shift m (Fork x l r) = Fork (x+m) (shift m l) (shift m r)

--- Define the preorder traversal, taken from /LectureNotes/data.md#traversals.
treePreOrder :: BT a -> [a]
treePreOrder Empty        = []
treePreOrder (Fork x l r) = [x] ++ treePreOrder l ++ treePreOrder r

--- In the example tree above, we get the linearSort able permutations
--- [2,1,3], [3,1,2] and [1,3,2].
fromBin :: Bin -> [Int]
fromBin = treePreOrder . toBT

--- One can also define this in one go (i.e. without going through BT Int).
fromBin_1 :: Bin -> [Int]
fromBin_1 L         = []
fromBin_1 (B t1 t2) = [n+1] ++ fromBin t1 ++ map (+(n+1)) (fromBin t2)
 where
   n = noOfBs t1

--- One half of an alternative bijection is given by the following function.
fromBin_alt :: Bin -> [Int]
fromBin_alt L         = []
fromBin_alt (B t1 t2) = fromBin_alt t1 ++ [n+m+1] ++ map (+n) (fromBin_alt t2)
 where
   n = noOfBs t1
   m = noOfBs t2

{- This alternative mapping from binary trees to permutations is actually
   equivalent to first labelling the nodes by a *postorder* traversal, e.g.

      *                 3
     / \               / \
    *   *     --->    1   2 
   / \ / \           / \ / \

   and

       *                 3
      / \               / \
     /   \             /   \
    *                 2
   / \        --->   / \
      *                 1
     / \               / \

   and

     *                 3
    / \               / \
   /   \             /   \
        *                 2
       / \    --->       / \
      *                 1
     / \               / \

   and then reading them back in an in-order traversal. -}

-- Question 5b (toBin)
toBin :: [Int] -> Maybe Bin
toBin xs | linearSort xs == [1..(length xs)] = Just (toBin' xs)
         | otherwise                         = Nothing

--- Reconstruct the tree from a preorder traversal of nodes labelled from left
--- to right.
toBin' :: [Int] -> Bin
toBin' []     = L
toBin' (x:xs) = B (toBin' smaller) (toBin' bigger)
 where
   smaller = [y | y <- xs, y < x]
   bigger  = [y | y <- xs, y > x]

{- Alternatively, we may check if the list has the right shape without relying
   on linearSort, as follows. The labelling of the tree (left-to-right) forms a
   labelled binary tree where for any fork, all the labels in the left-subtree
   must be smaller than the central node while those in the right-subtree must
   be larger. Therefore we can split the list into two halfs w.r.t the first
   element and transform them into binary trees.
   Note that we use some monad/bind notation (>>=) below.                    -}
toBin_1 :: [Int] -> Maybe Bin
toBin_1 []       = Just L
toBin_1 (x : xs) = 
  case findIndex (\y -> y > x) xs of 
    Just i -> let (ys, zs) = splitAt i xs in 
              if any (\z -> z < x) zs 
              then Nothing
              else 
                toBin_1 ys >>= \l ->
                toBin_1 zs >>= \r ->
                return (B l r)
    Nothing -> toBin_1 xs >>= \l -> 
               return (B l L)


--- The other half of the alternative bijection, again using monadic syntax.
--- Here the condition we need to check is that the elements to the left of
--- the maximum element are greater than the elements to the right.
toBin'_alt :: [Int] -> Maybe Bin
toBin'_alt []  = return L
toBin'_alt xs  = do
  let indexOfMax = fromJust (elemIndex (maximum xs) xs)
  let (left,_:right) = splitAt indexOfMax xs
  if not (null [(x,y) | x <- left, y <- right, x > y]) then Nothing else do
    l <- toBin'_alt left
    r <- toBin'_alt right
    return (B l r)
