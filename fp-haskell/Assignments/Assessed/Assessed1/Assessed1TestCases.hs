module Assessed1TestCases where

import Data.List
import Test.QuickCheck
import Control.DeepSeq

import Types
import qualified Assessed1Solutions as Sample
import qualified Assessed1 as Student

-- helper functions
permOf :: Eq a => [a] -> [a] -> Bool
permOf [] [] = True
permOf [] _ = False
permOf (x:xs) ys = x `elem` ys && xs `permOf` (delete x ys)

permOf' :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
permOf' xs ys = counterexample ("Your output " ++ show xs ++ "is not the same as " ++ show ys ++ ".") (xs `deepseq` ys `deepseq` (permOf xs ys))

ordPermOf :: (Ord a, Show a, NFData a) => [a] -> [a] -> Property
ordPermOf xs ys = sort xs ~= sort ys

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

subset' :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
subset' xs ys = counterexample ("Your output " ++ show xs ++ "is not a subset of " ++ show ys ++ ".") (xs `deepseq` ys `deepseq` (subset xs ys))

-- subset', but with arguments flipped in the output
subset'' :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
subset'' xs ys = counterexample ("Your output " ++ show ys ++ "is not a subset of " ++ show xs ++ ".") (xs `deepseq` ys `deepseq` (subset xs ys))

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted ([x]) = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

isBounded :: Spec -> Int -> Bool
isBounded (HasCup k _) n = 0 <= k && k < n
isBounded (Not s) n = isBounded s n
isBounded (Or s1 s2) n = isBounded s1 n && isBounded s2 n
isBounded (And s1 s2) n = isBounded s1 n && isBounded s2 n

-- better version of ===
infix 4 ~=
(~=) :: (Eq a, Show a, NFData a) => a -> a -> Property
x ~= y =
  counterexample ("Your output was " ++ show x ++ ", but the correct output is " ++ show y ++ ".")
  (deepseq x x == deepseq y y)

-- tests for doubleList
prop_doubleList_sol :: [Int] -> Property
prop_doubleList_sol xs = Student.doubleList xs ~= Sample.doubleList xs

prop_doubleList_empty :: Property
prop_doubleList_empty = doubleList' [] ~= []
  where
    doubleList' :: [Int] -> [Int]
    doubleList' = Student.doubleList

prop_doubleList_length :: [Int] -> Property
prop_doubleList_length xs = (length $ Student.doubleList xs) ~= 2 * length xs

prop_doubleList_subset :: [Int] -> Property
prop_doubleList_subset xs = (Student.doubleList xs) `subset'` xs

-- tests for firstDoubled
prop_firstDoubled_sol :: [Int] -> Property
prop_firstDoubled_sol xs = Student.firstDoubled xs ~= Sample.firstDoubled xs

prop_firstDoubled_any_double :: [Int] -> Property
prop_firstDoubled_any_double xs =
  counterexample ("Your output was " ++ show (Student.firstDoubled xs) ++
                  ", but this element is not a double element in " ++ show xs  ++ ".")
  $
  case Student.firstDoubled xs of
    Just x -> isDouble x xs
    Nothing -> True
  where
    isDouble :: Eq a => a -> [a] -> Bool
    isDouble a [] = False
    isDouble a ([x]) = False
    isDouble a (x:y:xs) = (x == y && a == x) || isDouble a (y:xs)

prop_firstDoubled_empty :: Property
prop_firstDoubled_empty = firstDoubled' [] ~= Nothing
  where
    firstDoubled' :: [Int] -> Maybe Int
    firstDoubled' = Student.firstDoubled

prop_firstDoubled_twice :: [Int] -> Property
prop_firstDoubled_twice xs =
  counterexample ("Your output was " ++ show (Student.firstDoubled xs) ++
                  ", but this element does not occur twice in " ++ show xs  ++ ".") (
  case Student.firstDoubled xs of
    Just x -> x `elem` (delete x xs) 
    Nothing -> True)

-- tests for priceRange
prop_priceRange_subset :: Price -> Price -> [Cupcake] -> Property
prop_priceRange_subset minP maxP cs = (Student.priceRange minP maxP cs) `subset'` cs

prop_priceRange_in_sol :: Price -> Price -> [Cupcake] -> Property
prop_priceRange_in_sol minP maxP cs = (Student.priceRange minP maxP cs) `subset'` (Sample.priceRange minP maxP cs)

prop_priceRange_sol_in :: Price -> Price -> [Cupcake] -> Property
prop_priceRange_sol_in minP maxP cs = (Sample.priceRange minP maxP cs) `subset''` (Student.priceRange minP maxP cs)

-- tests for allergyFree
prop_allergyFree_subset :: [Ingredient] -> [Cupcake] -> Property
prop_allergyFree_subset r cs = (Student.allergyFree r cs) `subset'` cs

prop_allergyFree_in_sol :: [Ingredient] -> [Cupcake] -> Property
prop_allergyFree_in_sol r cs = (Student.allergyFree r cs) `subset'` (Sample.allergyFree r cs)

prop_allergyFree_sol_in :: [Ingredient] -> [Cupcake] -> Property
prop_allergyFree_sol_in r cs = (Sample.allergyFree r cs) `subset''` (Student.allergyFree r cs) 


-- tests for checkSpec
-- make sure all the inputs are valid, i.e. 0 <= k < length t
prop_checkSpec_sol :: Spec -> Tin -> Property
prop_checkSpec_sol s t =
  isBounded s (length t) ==> Student.checkSpec s t ~= Sample.checkSpec s t
  
prop_checkSpec_and :: Spec -> Spec -> Tin -> Property
prop_checkSpec_and s1 s2 t =
  isBounded s1 (length t) && isBounded s2 (length t) ==>
  Student.checkSpec (And s1 s2) t ~= (Student.checkSpec s1 t && Student.checkSpec s2 t)

prop_checkSpec_or :: Spec -> Spec -> Tin -> Property
prop_checkSpec_or s1 s2 t =
  isBounded s1 (length t) && isBounded s2 (length t) ==>
  Student.checkSpec (Or s1 s2) t ~= (Student.checkSpec s1 t || Student.checkSpec s2 t)

prop_checkSpec_not :: Spec -> Tin -> Property
prop_checkSpec_not s t =
  isBounded s (length t) ==> Student.checkSpec (Not s) t ~= not (Student.checkSpec s t)



-- tests for checkSpec'
prop_checkSpec'_sol :: Spec -> Tin -> Property
prop_checkSpec'_sol s t = Student.checkSpec' s t ~= Sample.checkSpec' s t

-- make sure in all inputs, k < length t
prop_checkSpec'_geq :: Spec -> Tin -> Property
prop_checkSpec'_geq s t =
  isUpperBounded s ==> Student.checkSpec' s t ~= checkSpec' s t
  where
    isUpperBounded :: Spec -> Bool
    isUpperBounded (And s1 s2) = isUpperBounded s1 && isUpperBounded s2
    isUpperBounded (Or s1 s2) = isUpperBounded s1 && isUpperBounded s2
    isUpperBounded (Not s) = isUpperBounded s
    isUpperBounded (HasCup k _) = k < length t
    checkSpec' :: Spec -> Tin -> Maybe Bool
    checkSpec' (And s1 s2) t = checkSpec' s1 t >>= \x ->
                               checkSpec' s2 t >>= \y ->
                               return (x && y)
    checkSpec' (Or s1 s2) t = checkSpec' s1 t >>= \x ->
                              checkSpec' s2 t >>= \y ->
                              return (x || y)
    checkSpec' (Not s) t = checkSpec' s t >>= \x ->
                           return (not x)
    checkSpec' (HasCup k x) t = if k >= 0
                                then return (elem x (t !! k))
                                else Nothing

-- make sure in all inputs, k >= 0
prop_checkSpec'_lt :: Spec -> Tin -> Property
prop_checkSpec'_lt s t =
  isLowerBounded s ==> Student.checkSpec' s t ~= checkSpec' s t
  where
    isLowerBounded :: Spec -> Bool
    isLowerBounded (And s1 s2) = isLowerBounded s1 && isLowerBounded s2
    isLowerBounded (Or s1 s2) = isLowerBounded s1 && isLowerBounded s2
    isLowerBounded (Not s) = isLowerBounded s
    isLowerBounded (HasCup k _) = k >= 0
    checkSpec' :: Spec -> Tin -> Maybe Bool
    checkSpec' (And s1 s2) t = checkSpec' s1 t >>= \x ->
                               checkSpec' s2 t >>= \y ->
                               return (x && y)
    checkSpec' (Or s1 s2) t = checkSpec' s1 t >>= \x ->
                              checkSpec' s2 t >>= \y ->
                              return (x || y)
    checkSpec' (Not s) t = checkSpec' s t >>= \x ->
                           return (not x)
    checkSpec' (HasCup k x) t = if k < length t
                                then return (elem x (t !! k))
                                else Nothing



-- tests for linearSort
prop_linearSort_sol :: [Int] -> Property
prop_linearSort_sol xs = Student.linearSort xs ~= Sample.linearSort xs

prop_linearSort_perm :: [Int] -> Property
prop_linearSort_perm xs = Student.linearSort xs `ordPermOf` xs



-- tests for counterexample
prop_counterexample :: Property
prop_counterexample = property (Student.counterexample == [2,3,1])
--(not $ isSorted $ Sample.linearSort Student.counterexample)



-- tests for bin tree
prop_fromBin_validOutput :: Bin -> Property
prop_fromBin_validOutput t = (Sample.linearSort $ Student.fromBin t) ~= (sort $ Sample.fromBin t)

prop_toBin_validOutput :: [Int] -> Property
prop_toBin_validOutput xs = xs /= [] ==> if (isSorted $ Sample.linearSort xs)
                            then case Student.toBin xs of
                                   Nothing -> counterexample ("On the linearsortable list " ++ show xs ++ " you returned Nothing.") False
                                   Just t  ->
                                     counterexample ("On the linearsortable list " ++ show xs ++ " you returned a tree with the wrong number of nodes.")
                                     (length (Sample.fromBin t) == length xs)
                            else case Student.toBin xs of
                                   Nothing -> property True
                                   Just _  -> counterexample ("On the non-linearsortable list " ++ show xs ++ " you did not return Nothing") False

prop_fromBin_toBin_id :: [Int] -> Property
prop_fromBin_toBin_id xs = 
  xs /= [] && isSorted (Sample.linearSort xs) ==> (Student.fromBin <$> Student.toBin xs) ~= Just xs

prop_toBin_fromBin_id :: Bin -> Property
prop_toBin_fromBin_id t = Student.toBin (Student.fromBin t) ~= Just t

prop_fromBin_injective :: Bin -> Bin -> Property
prop_fromBin_injective t1 t2 =
  t1 /= t2 ==> counterexample ("Your fromBin is not injective, i.e. fromBin " ++ show t1 ++ " == " ++ "fromBin " ++ show t2 ++ ", but the inputs are not equal.") (Student.fromBin t1 /= Student.fromBin t2)

prop_toBin_injective :: [Int] -> [Int] -> Property
prop_toBin_injective xs ys =
  xs /= [] && isSorted (Sample.linearSort xs) && isSorted (Sample.linearSort ys) && xs /= ys ==>
  counterexample ("Your toBin is not injective, i.e. toBin " ++ show xs ++ " == " ++ "toBin " ++ show ys ++ ", but the inputs are not equal.") (Student.toBin xs /= Student.toBin ys)

prop_fromBinToBin_empty :: Property
prop_fromBinToBin_empty =
  counterexample "fromBin L should be [] and toBin L should be []." (Student.fromBin L == [] && Student.toBin [] == Just L)

