{-# LANGUAGE FlexibleInstances #-}

module Assessed2TestCases where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.State

import Types
import qualified Assessed2SolutionsForTesting as Sample
import qualified Assessed2 as Student
{-
We cannot import two modules with instances for the same class and same type,
so I created "Assessed2SolutionsForTesting" with _show_xxx functions.
-}

newtype Rank' = Rank' Rank
newtype Suit' = Suit' Suit
newtype Card' = Card' Card

instance Show Rank' where
  show (Rank' R2)  = "R2"
  show (Rank' R3)  = "R3"
  show (Rank' R4)  = "R4"
  show (Rank' R5)  = "R5"
  show (Rank' R6)  = "R6"
  show (Rank' R7)  = "R7"
  show (Rank' R8)  = "R8"
  show (Rank' R9)  = "R9"
  show (Rank' R10) = "R10"
  show (Rank' RJ)  = "RJ"
  show (Rank' RQ)  = "RQ"
  show (Rank' RK)  = "RK"
  show (Rank' RA)  = "RA"

instance Show Suit' where
  show (Suit' C) = "C"
  show (Suit' D) = "D"
  show (Suit' H) = "H"
  show (Suit' S) = "S"

instance Show Card' where
  show (Card' c) = "Card " ++ show (Rank' (rank c)) ++ " " ++ show (Suit' (suit c))

instance Arbitrary Rank' where
  arbitrary = Rank' <$> elements [R2 .. RA]

instance Arbitrary Suit' where
  arbitrary = Suit' <$> elements [C .. S]

instance Arbitrary Card' where
  arbitrary = do
    Rank' r <- arbitrary
    Suit' s <- arbitrary
    return $ Card' (Card r s)

------------------------------------------------------------------------------------------------------
instance PickingMonad Identity where
  pick lo hi = Identity lo

instance NFData (Bin a) where
  rnf (L x) = seq x ()
  rnf (B l r) = let _ = rnf l in rnf r

instance Ord a => Ord (Bin a) where
  compare (L x) (L y) = compare x y
  compare (L x) (B l r) = LT
  compare (B l r) (L x) = GT
  compare (B l1 r1) (B l2 r2) =
    case (compare l1 l2, compare r1 r2) of
      (EQ, r) -> r
      (GT, _) -> GT
      (LT, _) -> LT
  (<=) = \x y -> compare x y == LT || compare x y == EQ

instance PickingMonad (State s) where
  pick lo hi = return $ fromIntegral ((lo + hi + 1) `div` 2)

sortNormalise :: Ord a => Dist a -> Dist a
sortNormalise dist =
  let Dist ys = Sample.normalise dist in Dist (sort ys)

------------------------------------------------------------------------------------------------------
-- better version of ===
infix 4 ~=
(~=) :: (Eq a, Show a, NFData a) => a -> a -> Property
x ~= y =
  counterexample ("Your output was " ++ show x ++ ", but the correct output is " ++ show y ++ ".")
  (deepseq x x == deepseq y y)

permOf :: (Eq a, Ord a) => [a] -> [a] -> Bool
permOf xs ys = sort xs == sort ys

-- use xs `p_permOf` ys instead of sort xs ~= sort ys because the later prints sorted list as counter example
p_permOf :: (Eq a, Ord a, Show a, NFData a) => [a] -> [a] -> Property
p_permOf xs ys =
  counterexample ("Your output " ++ show xs ++ " is not a permutation of " ++ show ys ++ ".")
  (deepseq xs xs `permOf` deepseq ys ys)

p_isInfixOf :: String -> String -> Property
p_isInfixOf x y = counterexample ("Your output " ++ x ++ " is not a substring of " ++ y ++ ".")
                  (deepseq x x `isInfixOf` deepseq y y)

p_contain :: String -> String -> Property
p_contain x y = counterexample ("Your output " ++ x ++ " does not contain " ++ y ++ ".")
                (deepseq y y `isInfixOf` deepseq x x)

p_elem :: (Eq a, Show a, NFData a) => a -> [a] -> Property
p_elem x xs = counterexample ("Your output " ++ show x ++ " is not a member of " ++ show xs ++ ".")
             (deepseq x x `elem` deepseq xs xs)

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

p_subset :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
p_subset xs ys = counterexample ("Your output " ++ show xs ++ " is not a subset of " ++ show ys ++ ".")
                 (xs `deepseq` ys `deepseq` (xs `subset` ys))

p_superset :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
p_superset xs ys = counterexample ("Your output " ++ show xs ++ " is not a superset of " ++ show ys ++ ".")
                   (xs `deepseq` ys `deepseq` (ys `subset` xs))

isInterleave :: Eq a => [a] -> ([a], [a]) -> Bool
isInterleave [] ([], []) = True
isInterleave zs ([], ys) = ys == zs
isInterleave zs (xs, []) = xs == zs
isInterleave (z:zs) (x:xs, y:ys) =
  (x == z && isInterleave zs (xs, (y:ys))) || (y == z && isInterleave zs ((x:xs), ys))

p_isInterleave :: (Eq a, Show a, NFData a) => [a] -> ([a], [a]) -> Property
p_isInterleave zs (xs, ys) =
  counterexample ("Your output " ++ show zs ++ " is not an interleaving of (" ++ show xs ++ ", " ++ show ys ++ ").")
  (xs `deepseq` ys `deepseq` zs `deepseq` (zs `isInterleave` (xs, ys)))
------------------------------------------------------------------------------------------------------


-- Q1
-- show Rank
prop_showRank_correctness :: Rank' -> Property
prop_showRank_correctness (Rank' r) = show r ~= Sample._show_rank r

prop_showRank_2to10 :: Rank' -> Property
prop_showRank_2to10 (Rank' r) = r <= R10 ==> show r ~= Sample._show_rank r

prop_showRank_JtoA :: Rank' -> Property
prop_showRank_JtoA (Rank' r) = r >= RJ ==> show r ~= Sample._show_rank r

-- show Suit
prop_showSuit_correctness :: Suit' -> Property
prop_showSuit_correctness (Suit' s) = show s ~= Sample._show_suit s

prop_showSuit_missing_backslash :: Suit' -> Property
prop_showSuit_missing_backslash (Suit' s) = show (show s) `p_contain` Sample._show_suit' s

-- show Card
prop_showCard_correctness :: Card' -> Property
prop_showCard_correctness (Card' c) = show c ~= Sample._show_card c

prop_showCard_red :: Card' -> Property
prop_showCard_red (Card' c@(Card r s)) = s == D || s == H ==> show c ~= Sample._show_card c

prop_showCard_black :: Card' -> Property
prop_showCard_black (Card' c@(Card r s)) = s == C || s == S ==> show c ~= Sample._show_card c

prop_showCard_rank_sub :: Card' -> Property
prop_showCard_rank_sub (Card' c@(Card r s)) = show r `p_isInfixOf` show c

prop_showCard_suit_sub :: Card' -> Property
prop_showCard_suit_sub (Card' c@(Card r s)) = show s `p_isInfixOf` show c
------------------------------------------------------------------------------------------------------


-- Q2
-- choose
-- IO monad
prop_choose_io :: [Int] -> Property
prop_choose_io xs = xs /= [] ==> monadicIO $ do
  x <- run (Student.choose xs)
  monitor (\_ -> x `p_elem` xs)

-- List monad
prop_choose_list_sub :: [Int] -> Property
prop_choose_list_sub xs = xs /= [] ==>
  Student.choose xs `p_subset` xs

prop_choose_list_sup :: [Int]-> Property
prop_choose_list_sup xs = xs /= [] ==>
  Student.choose xs `p_superset` xs

-- Dist monad
prop_choose_dist :: [Int] -> Property
prop_choose_dist xs = xs /= [] ==>
  let Dist x = sortNormalise $ Student.choose xs in
  let Dist y = sortNormalise $ Sample.choose xs in
  x ~= y

-- simulate
-- IO monad
prop_simulate_io :: Bool -> Integer -> Property
prop_simulate_io b i = i >= 0 ==> monadicIO $ do
  r <- run (Student.simulate (return b) i)
  monitor (\_ -> counterexample ("Your output " ++ show r ++ " is not between 0 and " ++ show i ++ ".")
                 (r `deepseq` r <= i && r >= 0))

prop_simulate_id_true :: Integer -> Property
prop_simulate_id_true i = i >= 0 ==> monadic runIdentity $ do
  r <- run (Student.simulate (return True) i)
  monitor (\_ -> r ~= i)

prop_simulate_id_false :: Integer -> Property
prop_simulate_id_false i = i >= 0 ==> monadic runIdentity $ do
  r <- run (Student.simulate (return False) i)
  monitor (\_ -> r ~= 0)

prop_simulate_dist :: Integer -> Property
prop_simulate_dist i = i >= 0 && i <= 10 ==> -- the list gets really long when i > 10
  let Dist x = sortNormalise $ Student.simulate (Sample.choose [True,False] :: Dist Bool) i in
  let Dist y = sortNormalise $ Sample.simulate (Sample.choose [True,False] :: Dist Bool) i in
  x ~= y

prop_simulate_list :: Integer -> Property
prop_simulate_list i = i >= 0 && i <= 10 ==> -- the list gets really long when i > 10
  Student.simulate (Sample.choose [True,False] :: [Bool]) i ~= Sample.simulate (Sample.choose [True,False] :: [Bool]) i
------------------------------------------------------------------------------------------------------


-- Q3
-- cut
prop_cut_id :: Property
prop_cut_id = let Identity x = Student.cut [] in (x :: ([Int],[Int])) ~= ([], [])

prop_cut_io :: [Int] -> Property
prop_cut_io xs = xs /= [] ==> monadicIO $ do
  (ys, zs) <- run (Student.cut xs)
  monitor (\_ -> ys ++ zs ~= xs)

prop_cut_list_sub :: [Int] -> Property
prop_cut_list_sub xs = xs /= [] ==> Student.cut xs `p_subset` Sample.cut xs

prop_cut_list_sup :: [Int] -> Property
prop_cut_list_sup xs = xs /= [] ==> Student.cut xs `p_superset` Sample.cut xs

prop_cut_dist :: [Int] -> Property
prop_cut_dist xs = xs /= [] ==>
  let Dist x = sortNormalise $ Student.cut xs in
  let Dist y = sortNormalise $ Sample.cut xs in
  x ~= y

-- shuffle
prop_shuffle_id_l :: [Int] -> Property
prop_shuffle_id_l xs = xs /= [] ==>
  let Identity ys = Student.shuffle ([], xs) in
  ys ~= xs

prop_shuffle_id_r :: [Int] -> Property
prop_shuffle_id_r xs = xs /= [] ==>
  let Identity ys = Student.shuffle (xs, []) in
  ys ~= xs

prop_shuffle_id_both :: Property
prop_shuffle_id_both =
  let Identity xs = Student.shuffle ([], []) in
  xs ~= ([] :: [Int])

-- if it is too slow, try only generates sets
prop_shuffle_io :: ([Int], [Int]) -> Property
prop_shuffle_io deck = fst deck /= [] && snd deck /= [] ==> monadicIO $ do
  zs <- run (Student.shuffle deck)
  monitor (\_ -> zs `p_isInterleave` deck)

prop_shuffle_list_sub :: ([Int], [Int]) -> Property
prop_shuffle_list_sub deck = fst deck /= [] && snd deck /= [] ==>
  let xs = Student.shuffle deck in
  let ys = Sample.shuffle deck in
  xs `p_subset` ys

prop_shuffle_list_sup :: ([Int], [Int]) -> Property
prop_shuffle_list_sup deck = fst deck /= [] && snd deck /= [] ==>
  let xs = Student.shuffle deck in
  let ys = Sample.shuffle deck in
  xs `p_superset` ys

prop_shuffle_dist :: ([Int], [Int]) -> Property
prop_shuffle_dist deck = fst deck /= [] && snd deck /= [] ==>
  let Dist xs = sortNormalise $ Student.shuffle deck in
  let Dist ys = sortNormalise $ Sample.shuffle deck in
  xs ~= ys
------------------------------------------------------------------------------------------------------


prop_riffles_id_empty :: Int -> Property
prop_riffles_id_empty n = n >= 0 ==>
  let Identity xs = Student.riffles Sample.cut Sample.shuffle n [] in
  xs ~= ([] :: [Int])

prop_riffles_id_0 :: [Int] -> Property
prop_riffles_id_0 xs = xs /= [] ==>
  let Identity ys = Student.riffles Sample.cut Sample.shuffle 0 xs in
  ys ~= xs

prop_riffles_io :: Int -> [Int] -> Property
prop_riffles_io n xs = n >= 0 && xs /= [] ==> monadicIO $ do
  r <- run (Student.riffles Sample.cut Sample.shuffle n xs)
  monitor (\_ -> r `p_permOf` xs)

prop_riffles_dist :: Int -> [Int] -> Property
prop_riffles_dist n xs =
  n >= 0 && xs /= [] ==>
  let Dist x = sortNormalise $ Student.riffles Sample.cut Sample.shuffle n xs in
  let Dist y = sortNormalise $ Sample.riffles Sample.cut Sample.shuffle n xs in
  x ~= y

prop_riffles_num_calls_cut :: Int -> [Int] -> Property
prop_riffles_num_calls_cut n xs =
  n >= 0 && xs /= [] ==>
  monadic (\ms -> fst $ runState ms 0) $ do
    run $ Student.riffles (\xs -> modify (+1) >> Sample.cut xs) (Sample.shuffle) n xs
    i <- run get
    monitor (\_ -> i ~= n)

prop_riffles_num_calls_shuffle :: Int -> [Int] -> Property
prop_riffles_num_calls_shuffle n xs =
  n >= 0 && xs /= [] ==>
  monadic (\ms -> fst $ runState ms 0) $ do
    run $ Student.riffles Sample.cut (\xs -> modify (+1) >> Sample.shuffle xs) n xs
    i <- run get
    monitor (\_ -> i ~= n)

-- Q4
prop_permute_id :: Property
prop_permute_id =
  let Identity r = Student.permute [] in
  r ~= ([] :: [Int])

prop_permute_io :: [Int] -> Property
prop_permute_io xs = xs /= [] ==> monadicIO $ do
  r <- run (Student.permute xs)
  monitor (\_ -> r `p_permOf` xs)

prop_permute_list :: [Int] -> Property
prop_permute_list xs = xs /= [] ==> Student.permute xs `p_permOf` Sample.permute xs

prop_permute_list_sup :: [Int] -> Property
prop_permute_list_sup xs = xs /= [] ==> Student.permute xs `p_superset` Sample.permute xs

prop_permute_dist :: [Int] -> Property
prop_permute_dist xs = xs /= [] ==>
  let Dist x = sortNormalise $ Student.permute xs in
  let Dist y = sortNormalise $ Sample.permute xs in
  x ~= y
------------------------------------------------------------------------------------------------------


-- Q5
prop_genTree_id :: Int -> Property
prop_genTree_id x =
  let Identity t = Student.genTree [x] in
  t ~= L x

prop_genTree_io :: [Int] -> Property
prop_genTree_io xs = length xs > 1 ==> monadicIO $ do
  r <- run (Student.genTree xs)
  monitor (\_ -> canopy r `p_permOf` xs)
  where canopy :: Bin a -> [a]
        canopy (L x) = [x]
        canopy (B l r) = canopy l ++ canopy r

prop_genTree_list :: [Int] -> Property
prop_genTree_list xs = length xs > 1 ==>
  Student.genTree xs `p_permOf` Sample.genTree xs

prop_genTree_list_sup :: [Int] -> Property
prop_genTree_list_sup xs = length xs > 1 ==>
  Student.genTree xs `p_superset` Sample.genTree xs

prop_genTree_dist :: [Int] -> Property
prop_genTree_dist xs = length xs > 1 ==>
  let Dist x = sortNormalise $ Student.genTree xs in
  let Dist y = sortNormalise $ Sample.genTree xs in
  x ~= y
