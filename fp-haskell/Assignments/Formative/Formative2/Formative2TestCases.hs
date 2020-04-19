module Formative2TestCases where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.DeepSeq
import Control.Monad.Identity

import Types
import qualified Formative2Solutions as Sample
import qualified Formative2 as Student

-- better version of ===
infix 4 ~=
(~=) :: (Eq a, Show a, NFData a) => a -> a -> Property
x ~= y =
  counterexample ("Your output was " ++ show x ++ ", but the correct output is " ++ show y ++ ".")
  (deepseq x x == deepseq y y)


-- prop for canopy
prop_canopy :: Tree Int Int -> Property
prop_canopy t = Student.canopy t ~= Sample.canopy t


-- prop for treePreOrder
prop_treePreOrder :: Tree Int Int -> Property
prop_treePreOrder t = Student.treePreOrder t ~= Sample.treePreOrder t


-- prop for eval
prop_eval :: OpTree Int -> Property
prop_eval t = Student.eval t ~= Sample.eval t


-- prop for qsort
badList :: [Int]
badList = [10000, 9999..1]

prop_qsort_id_correctness :: [Int] -> Property
prop_qsort_id_correctness xs = monadic runIdentity $ do
  ys <- run (Student.qsort xs)
  zs <- run (Sample.qsort xs)
  monitor (\_ -> ys ~= sort xs)

prop_qsort_id_time :: Property
prop_qsort_id_time = monadic runIdentity $ do
  ys <- run (Student.qsort badList)
  monitor (\_ -> ys ~= ys)

prop_qsort_io_correctness :: [Int] -> Property
prop_qsort_io_correctness xs = monadicIO $ do
  ys <- run (Student.qsort xs)
  monitor (\_ -> ys ~= sort xs)

prop_qsort_io_time :: Property
prop_qsort_io_time = monadicIO $ do
  ys <- run (Student.qsort badList)
  monitor (\_ -> ys ~= ys)
