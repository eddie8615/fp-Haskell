module Formative2Marking where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.Random

import Types
import qualified Formative2Solutions as Sample
import Formative2TestCases hiding (permOf, ordPermOf, isSorted, isBounded)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tree a b) where
  arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: (Arbitrary a, Arbitrary b) => Int -> Gen (Tree a b)
arbitrarySizedTree 0 = Leaf <$> arbitrary
arbitrarySizedTree n = do
  k <- choose (0, n)
  frequency [
    (1, Leaf <$> arbitrary),
    (4, Node <$> arbitrary <*> vectorOf k (arbitrarySizedTree (n `div` k)))]

instance Arbitrary Op where
  arbitrary = elements [Add, Mul]

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

hideMarks = True

main :: IO ()
main = do
  feedback <- getArgs >>= return . not . ("--marking" `elem`)
  marks <- runAllTests feedback tests
  when (not hideMarks) $
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
  test_canopy,
  test_treePreOrder,
  test_eval,
  test_qsort_io,
  test_qsort_io_time,
  test_qsort_id,
  test_qsort_id_time]

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

test_canopy = Test {
  mark = 0,
  description = "Checking if canopy is correct...",
  successMsg = "canopy is correct.",
  failMsg = "canopy is not correct.",
  test = test1 prop_canopy,
  partialTests = []
}

test_treePreOrder = Test {
  mark = 0,
  description = "Checking if treePreeOrder is correct...",
  successMsg = "treePreeOrder is correct.",
  failMsg = "treePreeOrder is not correct.",
  test = test1 prop_treePreOrder,
  partialTests = []
}

test_eval = Test {
  mark = 0,
  description = "Checking if eval is correct...",
  successMsg = "eval is correct.",
  failMsg = "eval is not correct.",
  test = test1 prop_eval,
  partialTests = []
}

test_qsort_io = Test {
  mark = 0,
  description = newSection ++ "Checking if qsort is correct on the IO monad...",
  successMsg = "qsort is correct on the IO monad.",
  failMsg = "qsort is not correct on the IO monad.",
  test = test0 prop_qsort_io_correctness,
  partialTests = []
}

test_qsort_io_time = Test {
  mark = 0,
  description = "Checking if qsort times out on a large reversed list on the IO monad...",
  successMsg = "qsort is probably O(n log n) on the IO monad.",
  failMsg = "qsort is not O(n log n) on the IO monad. You probably did not use the picking monad.",
  test = once $ test0 prop_qsort_io_time,
  partialTests = []
}

test_qsort_id = Test {
  mark = 0,
  description = "Checking if qsort is correct on the Identity monad...",
  successMsg = "qsort is correct on the Identity monad.",
  failMsg = "qsort is not correct on the Identity monad.",
  test = test0 prop_qsort_id_correctness,
  partialTests = []
}

test_qsort_id_time = Test {
  mark = 0,
  description = "Checking if qsort times out on a large reversed list on the Identity monad...",
  successMsg = "qsort is probably O(n^2) on the Identity monad.",
  failMsg = "qsort is not O(n^2) on the Identity monad. You probably did not use the picking monad.",
  test = expectFailure $ once $ test0 prop_qsort_id_time,
  partialTests = []
}
