module Assessed1Marking where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.Random

import Types
import qualified Assessed1Solutions as Sample
import Assessed1TestCases hiding (permOf, ordPermOf, isSorted, isBounded)

instance Arbitrary Ingredient where
  arbitrary = elements [Nuts, Gluten, Soy, Dairy]

instance Arbitrary Price where
  arbitrary = P <$> arbitrary `suchThat` (>= 0)

instance Arbitrary Cupcake where
  arbitrary = CC <$> arbitrary <*> arbitrary

instance Arbitrary Spec where
  arbitrary = sized arbitrarySizedSpec

-- Generate an arbitrary spec of the given size.
arbitrarySizedSpec :: Int -> Gen Spec
arbitrarySizedSpec 0 = HasCup <$> arbitrary `suchThat` (>= -1) <*> arbitrary
arbitrarySizedSpec n = frequency [
  (3, HasCup <$> arbitrary `suchThat` (>= -1) <*> arbitrary),
  (1, Not <$> arbitrarySizedSpec (n - 1)),
  (1, Or <$> arbitrarySizedSpec (n `div` 2) <*> arbitrarySizedSpec (n `div` 2)),
  (1, And <$> arbitrarySizedSpec (n `div` 2) <*> arbitrarySizedSpec (n `div` 2))]

instance Arbitrary Bin where
  arbitrary = sized arbitrarySizedBin
  shrink L = []
  shrink (B l r) = [L] ++ [l, r] ++ [B l' r' | (l', r') <- shrink (l, r)]

-- Generate an arbitrary binary tree of the given size.
arbitrarySizedBin :: Int -> Gen Bin
arbitrarySizedBin 0 = return L
arbitrarySizedBin n = frequency [
  (1, return L),
  (4, B <$> arbitrarySizedBin (n `div` 2) <*> arbitrarySizedBin (n `div` 2))]

type Permutation = [Int]

-- Generate an arbitrary stack-sortable permutation.
arbitraryPermutation :: Gen Permutation
arbitraryPermutation = Sample.fromBin <$> arbitrary

shrinkPermutation :: Permutation -> [Permutation]
shrinkPermutation = map Sample.fromBin . shrink . fromJust . Sample.toBin

-- Generate an arbitrary pair of two stack-sortable permutations.
arbitraryPairOfPermutations :: Gen (Permutation, Permutation)
arbitraryPairOfPermutations =
  arbitraryPermutation >>= (\xs -> arbitraryPermutation >>= (\ys -> return (xs, ys)))

shrinkPairOfPermutations :: (Permutation, Permutation) -> [(Permutation, Permutation)]
shrinkPairOfPermutations (xs, ys) = shrinkPermutation xs `zip` shrinkPermutation ys

data Test = Test {
  mark :: Int,
  description :: String,
  successMsg :: String,
  failMsg :: String,
  test :: Property,
  partialTests :: [Test] }

-- Test timeout in microseconds.
time :: Int
time = 1000000

-- Seed for quickcheck.
quickCheckSeed = Just (mkQCGen 28, 0)

main :: IO ()
main = do
  feedback <- getArgs >>= return . not . ("--marking" `elem`)
  marks <- runAllTests feedback tests
  putStrLn $ if feedback
             then newSection ++ newSection ++ newSection ++ (toMarks $ printMarks marks)
             else show marks

runTest :: Bool -> Test -> IO Int
runTest feedback x = do
  when feedback $ putStrLn $ toBold $ description x
  let quickCheckArgs = stdArgs { chatty = feedback, replay = quickCheckSeed, maxSize = 30 }
  let test' = counterexample "The test failed on input(s):" $ test x
  result <- quickCheckWithResult quickCheckArgs test'
  if isSuccess result
  then do
    when feedback $ putStrLn $ toCorrect (successMsg x) ++ "\n"
    return $ mark x
  else do
    when feedback $ putStrLn $ toFail (failMsg x) ++ if (null $ failMsg x) then "" else "\n"
    return 0

runAllTests :: Bool -> [Test] -> IO Int
runAllTests feedback = runAllTestsIter 0
  where
    runAllTestsIter :: Int -> [Test] -> IO Int
    runAllTestsIter marks [] = return marks
    runAllTestsIter marks (test:tests) =
      runTest feedback test >>= (\mark -> if mark == 0
        then runAllTestsIter marks (partialTests test ++ tests)
        else runAllTestsIter (mark + marks) tests)

-- Helper wrapper functions for tests.
test0 :: Testable prop => prop -> Property
test0 foo = within time foo

test1 :: (Arbitrary a, Show a, Testable prop) => (a -> prop) -> Property
test1 foo = property (\x -> within time $ foo x)

test2 :: (Arbitrary a, Show a, Arbitrary b, Show b, Testable prop) => (a -> b -> prop) -> Property
test2 foo = property (\x y -> within time $ foo x y)

test3 :: (Arbitrary a, Show a, Arbitrary b, Show b, Arbitrary c, Show c, Testable prop) => (a -> b -> c -> prop) -> Property
test3 foo = property (\x y z -> within time $ foo x y z)

testWith1 :: (Arbitrary a, Show a, Testable prop) => (a -> prop) -> Gen a -> (a -> [a]) -> Property
testWith1 foo gen shrink = forAllShrink gen shrink (\x -> within time $ foo x)

testWith2 :: (Arbitrary a, Show a, Arbitrary b, Show b, Testable prop) => (a -> b -> prop) -> Gen (a, b) -> ((a, b) -> [(a, b)]) -> Property
testWith2 foo gen shrink = forAllShrink gen shrink (\(x, y) -> within time $ foo x y)

testWith3 :: (Arbitrary a, Show a, Arbitrary b, Show b, Arbitrary c, Show c, Testable prop) => (a -> b -> c -> prop) -> Gen (a, b, c) -> ((a, b, c) -> [(a, b, c)]) -> Property
testWith3 foo gen shrink = forAllShrink gen shrink (\(x, y, z) -> within time $ foo x y z)

tests :: [Test]
tests = [
  test_doubleList,
  test_firstDoubled,
  test_priceRange_in_sol,
  test_priceRange_sol_in,
  test_allergyFree_in_sol,
  test_allergyFree_sol_in,
  test_checkSpec,
  test_checkSpec',
  test_linearSort,
  test_counterexample,
  test_fromBinToBin_empty,
  test_fromBin_validOutput,
  test_toBin_injective,
  test_toBin_fromBin_id,
  test_fromBin_toBin_id]

printMarks :: Int -> String
printMarks m | m < 4*6             = "Your mark is: " ++ show m ++ ". Keep at it! ;)"
             | 4*6 <= m && m < 5*6 = "Your mark is: " ++ show m ++ ". You are on the right track now! :)"
             | 5*6 <= m && m < 6*6 = "Your mark is: " ++ show m ++ ". Good effort! :)"
             | 6*6 <= m && m < 7*6 = "Your mark is: " ++ show m ++ ". Job well done! :)"
             | 7*6 <= m && m < 8*6 = "Your mark is: " ++ show m ++ ". Really nice work! :)"
             | 8*6 <= m && m < 9*6 = "Your mark is: " ++ show m ++ ". Terrific! :D"
             | 9*6 <= m && m < 60  = "Your mark is: " ++ show m ++ ". Very impresive, nearly perfect! :D"
             | m == 60             = "Your mark is: " ++ show m ++ ". Superb, absolutely perfect! :D"

toMarks :: String -> String
toMarks s = "\x1b[1m\x1b[34m" ++ s ++ "\x1b[0m"

toBold :: String -> String
toBold s = "\x1b[1m" ++ s ++ "\x1b[0m"

toCorrect :: String -> String
toCorrect s = "\x1b[1m\x1b[32m" ++ s ++ " :)" ++ "\x1b[0m"

toFail :: String -> String
toFail s = "\x1b[1m\x1b[31m" ++ s ++ "\x1b[0m"

newSection :: String
newSection = take 80 (repeat '-') ++ "\n"

-- Tests for doubleList.
test_doubleList = Test {
  mark = 10,
  description = newSection ++ "Checking if doubleList is correct...",
  successMsg = "doubleList is correct.",
  failMsg = "doubleList is not correct, checking for partial marks...",
  test = test1 prop_doubleList_sol,
  partialTests = [test_doubleList_empty, test_doubleList_length, test_doubleList_subset]
}

test_doubleList_empty = Test {
  mark = 2,
  description = "Checking if doubleList [] = []...",
  successMsg = "You got 2 marks for correctly handling the empty list case.",
  failMsg =  "doubleList [] should be [].",
  test = test0 prop_doubleList_empty,
  partialTests = []
}

test_doubleList_length = Test {
  mark = 2,
  description = "Checking if doubleList doubles the length of the list...",
  successMsg = "You got 2 marks for having doubleList double the amount of elements.",
  failMsg =  "doubleList should double the amount of elements.",
  test = test1 prop_doubleList_length,
  partialTests = []
}

test_doubleList_subset = Test {
  mark = 2,
  description = "Checking if doubleList xs only adds elements that were already in xs...",
  successMsg = "You got 2 marks for not adding elements to doubleList xs that were not already in xs.",
  failMsg = "doubleList xs should not introduce elements that were not already in xs.",
  test = test1 prop_doubleList_subset,
  partialTests = []
}

-- Tests for firstDoubled.
test_firstDoubled = Test {
  mark = 10,
  description = newSection ++ "Checking if firstDoubled is correct...",
  successMsg = "firstDoubled is correct.",
  failMsg = "firstDoubled is not correct, checking for partial marks...",
  test = test1 prop_firstDoubled_sol,
  partialTests = [test_firstDoubled_any_double, test_firstDoubled_empty, test_firstDoubled_twice]
}

test_firstDoubled_any_double = Test {
  mark = 2,
  description = "Checking if firstDoubled returns an element that is indeed doubled, but not necessarily the first...",
  successMsg = "You got 2 marks for returning a doubled element (that wasn't the first).",
  failMsg = "firstDoubled should return an element that is doubled in the list.",
  test = test1 prop_firstDoubled_any_double,
  partialTests = []
}

test_firstDoubled_empty = Test {
  mark = 2,
  description = "Checking if firstDoubled [] = Nothing...",
  successMsg = "You got 2 marks for having firstDoubled [] = Nothing.",
  failMsg = "firstDoubled [] should be Nothing.",
  test = test0 prop_firstDoubled_empty,
  partialTests = []
}

test_firstDoubled_twice = Test {
  mark = 2,
  description = "Checking if firstDoubled returns any element that occurs twice in the list...",
  successMsg = "You got 2 marks for returning any element that occurs twice in the list.",
  failMsg = "firstDoubled xs should return an element that occurs twice in xs.",
  test = test1 prop_firstDoubled_twice,
  partialTests = []
}

-- Tests for priceRange.
test_priceRange_subset = Test {
  mark = 2,
  description = "Checking if priceRange returns a subset of its input...",
  successMsg =  "You got 2 marks for returning a subset of the original list of cupcakes.",
  failMsg =  "priceRange minP maxP xs should only return cupcakes that are in xs.",
  test = test3 prop_priceRange_subset,
  partialTests = []
}

test_priceRange_in_sol = Test {
  mark = 5,
  description = "Checking if all of the cupcakes returned are within the correct price range...",
  successMsg =  "You got 5 marks for returning cupcakes that are all within the correct price range.",
  failMsg =  "priceRange minP maxP xs should return cupcakes that have a price between minP and maxP",
  test = test3 prop_priceRange_in_sol,
  partialTests = [test_priceRange_subset]
}

test_priceRange_sol_in = Test {
  mark = 5,
  description = "Checking if all of the cupcakes within the correct price range are returned...",
  successMsg = "You got 5 marks for including all of the cupcakes within the correct price range.",
  failMsg = "Every cupcake in xs that is within the price range should be an element of priceRange minP maxP xs.",
  test = test3 prop_priceRange_sol_in,
  partialTests = [test_priceRange_subset]
}

-- Tests for allergyFree.
test_allergyFree_subset = Test {
  mark = 2,
  description = "Checking if allergyFree returns a subset of its input...",
  successMsg =  "You got 2 marks for returning a subset of the original list of cupcakes.",
  failMsg =  "allergyFree r cs should only return cupcakes that are in cs.",
  test = test2 prop_allergyFree_subset,
  partialTests = []
}

test_allergyFree_in_sol = Test {
  mark = 5,
  description = "Checking if all of the cupcakes returned do not contain allergens...",
  successMsg =  "You got 5 marks for returning cupcakes that are all allergy free.",
  failMsg =  "allergyFree should return cupcakes that do not contain allergens.",
  test = test2 prop_allergyFree_in_sol,
  partialTests = [test_allergyFree_subset]
}

test_allergyFree_sol_in = Test {
  mark = 5,
  description = "Checking if all of the cupcakes without allergens are returned...",
  successMsg = "You got 5 marks for including all of the cupcakes without allergens.",
  failMsg = "Every cupcake in xs that does not contain allergens should be an element of priceRange minP maxP xs.",
  test = test2 prop_allergyFree_sol_in,
  partialTests = [test_allergyFree_subset]
}

-- Tests for checkSpec.
test_checkSpec = Test {
  mark = 5,
  description = newSection ++ "Checking if checkSpec is correct...",
  successMsg = "checkSpec is correct.",
  failMsg = "checkSpec is incorrect, checking for partial marks...",
  test = test2 prop_checkSpec_sol,
  partialTests = [test_checkSpec_and, test_checkSpec_or, test_checkSpec_not]
}

test_checkSpec_and = Test {
  mark = 1,
  description = "Checking if checkSpec functions correctly on And...",
  successMsg = "You got 1 mark for handling And correctly.",
  failMsg = "checkSpec did not handle And correctly.",
  test = test2 prop_checkSpec_and,
  partialTests = []
}

test_checkSpec_or = Test {
  mark = 1,
  description = "Checking if checkSpec functions correctly on Or...",
  successMsg = "You got 1 mark for handling Or correctly.",
  failMsg = "checkSpec did not handle Or correctly.",
  test = test2 prop_checkSpec_or,
  partialTests = []
}

test_checkSpec_not = Test {
  mark = 1,
  description = "Checking if checkSpec functions correctly on Not...",
  successMsg = "You got 1 mark for handling Not correctly.",
  failMsg = "checkSpec did not handle Not correctly.",
  test = test2 prop_checkSpec_not,
  partialTests = []
}

-- Tests for checkSpec'.
test_checkSpec' = Test {
  mark = 5,
  description = newSection ++ "Checking if checkSpec' is correct...",
  successMsg = "checkSpec' is correct.",
  failMsg = "checkSpec' is not correct, checking for partial marks...",
  test = test2 prop_checkSpec'_sol,
  partialTests = [test_checkSpec'_geq, test_checkSpec'_lt]
}

test_checkSpec'_geq = Test {
  mark = 2,
  description = "Checking if checkSpec' can cope with negative indices...",
  successMsg = "You got 2 marks as checkSpec' can cope with negative indices.",
  failMsg = "checkSpec' cannot cope with negative indices.",
  test = test2 prop_checkSpec'_geq,
  partialTests = []
}

test_checkSpec'_lt = Test {
  mark = 2,
  description = "Checking if checkSpec' can cope with indices that are too large...",
  successMsg = "You got 2 marks as checkSpec' can cope with indices that are too large.",
  failMsg = "checkSpec' cannot cope with indices that are too large.",
  test = test2 prop_checkSpec'_lt,
  partialTests = []
}

-- Tests for linearSort.
test_linearSort = Test {
  mark = 4,
  description = newSection ++ "Checking if linearSort is correct...",
  successMsg = "linearSort is correct.",
  failMsg = "linearSort is not correct, checking for partial marks...",
  test = test1 prop_linearSort_sol,
  partialTests = [test_linearSort_perm]
}

test_linearSort_perm = Test {
  mark = 2,
  description = "Checking if linearSort returns a permutation of its input...",
  successMsg = "You got 2 marks for returning a permutation of the input.",
  failMsg = "linearSort must return a permutation of its input.",
  test = test1 prop_linearSort_perm,
  partialTests = []
}

-- Test for counterexample.
test_counterexample = Test {
  mark = 1,
  description = "Checking your counterexample...",
  successMsg = "The counterexample was indeed [2,3,1].",
  failMsg = "Your counterexample should have been [2,3,1].",
  test = test0 prop_counterexample,
  partialTests = []
}

-- Test for bin tree
test_fromBin_validOutput = Test {
  mark = 1,
  description = "Checking that the output of fromBin is valid...",
  successMsg = "You got a mark because at least one of fromBin and toBin had valid output.",
  failMsg = "",
  test = test1 prop_fromBin_validOutput,
  partialTests = [test_toBin_validOutput]
}

test_toBin_validOutput = Test {
  mark = 1,
  description = "Checking that the output of toBin is valid...",
  successMsg = "You got a mark because one of fromBin and toBin had valid output.",
  failMsg = "Neither toBin or fromBin has valid output.",
  test = testWith1 prop_toBin_validOutput arbitraryPermutation shrinkPermutation,
  partialTests = []
}

test_fromBin_toBin_id = Test {
  mark = 1,
  description = "Checking applying toBin then fromBin to a linear sortable permutation is the identity...",
  successMsg = "You got a mark for this composition being the identity.",
  failMsg = "This composition was not the identity.",
  test = testWith1 prop_fromBin_toBin_id arbitraryPermutation shrinkPermutation,
  partialTests = []
}

test_toBin_fromBin_id = Test {
  mark = 1,
  description = "Checking applying fromBin then toBin is the identity...",
  successMsg = "You got a mark for this composition being the identity.",
  failMsg = "This composition was not the identity.",
  test = test1 prop_toBin_fromBin_id,
  partialTests = []
}

test_toBin_injective = Test {
  mark = 1,
  description = "Checking toBin is injective (i.e. different inputs should go to different outputs)...",
  successMsg = "You got a mark because at least one of fromBin and toBin is injective.",
  failMsg = "",
  test = testWith2 prop_toBin_injective arbitraryPairOfPermutations shrinkPairOfPermutations,
  partialTests = [test_fromBin_injective]
}

test_fromBin_injective = Test {
  mark = 1,
  description = "Checking fromBin is injective (i.e. different inputs should go to different outputs)...",
  successMsg = "You got a mark because at least one of fromBin and toBin is injective.",
  failMsg = "Both fromBin and toBin are not injective.",
  test = test2 prop_fromBin_injective,
  partialTests = []
}

test_fromBinToBin_empty = Test {
  mark = 1,
  description = newSection ++ "Checking fromBin and toBin work on the empty list/tree...",
  successMsg = "You got a mark because your functions work correctly on empty cases.",
  failMsg = "The functions do not handle the empty cases correctly.",
  test = test0 prop_fromBinToBin_empty,
  partialTests = []
}
