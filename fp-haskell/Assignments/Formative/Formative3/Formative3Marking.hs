module Formative3Marking where

import Data.Char
import Control.Monad
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.Random

import Formative3TestCases

-- Test timeout in microseconds.
time :: Int
time = 1000000

-- Whether the assignment is assesed.
isAssessed :: Bool
isAssessed = False

-- Seed for quickcheck. Do not change this!
quickCheckSeed :: Maybe (QCGen, Int)
quickCheckSeed = Just (mkQCGen 28, 0)

data Test = Test { mark :: Int
                 , description :: String
                 , successMsg :: String
                 , failMsg :: String
                 , test :: Property
                 , condition :: TestCondition
                 }

data TestCondition = Always
                   | IfFail Test
                   | IfSuccess Test
                   | Any [TestCondition]
                   | All [TestCondition]

instance Eq Test where
  t1 == t2 = description t1 == description t2

eval :: TestCondition -> [Test] -> Bool
eval (Always) successful = True
eval (IfFail test) successful = not $ test `elem` successful
eval (IfSuccess test) successful = test `elem` successful
eval (Any conds) successful = any (\cond -> eval cond tests) conds
eval (All conds) successful = all (\cond -> eval cond tests) conds

main :: IO ()
main = do
  feedback <- getArgs >>= return . not . ("--marking" `elem`)
  marks <- runAllTests feedback tests
  when isAssessed $ putStrLn $
    if feedback
      then newSection ++ newSection ++ newSection ++ (toMarks $ printMarks marks)
      else show marks

quickCheckArgs :: Bool -> Args
quickCheckArgs feedback =
  Args { replay = quickCheckSeed
       , maxSuccess = 100
       , maxDiscardRatio = 10
       , maxSize = 30
       , chatty = feedback
       , maxShrinks = 30
       }

runTest :: Bool -> Test -> IO Bool
runTest feedback x = do
  when feedback $ putStrLn $ toBold $ description x
  let test' = counterexample "The test failed on input(s):" $ test x
  result <- isSuccess <$> quickCheckWithResult (quickCheckArgs feedback) test'
  when feedback $ putStrLn $ if result
    then toCorrect (successMsg x) ++ "\n"
    else toFail (failMsg x) ++ if null (failMsg x) then "" else "\n"
  return result

runAllTests :: Bool -> [Test] -> IO Int
runAllTests feedback = runAllTestsIter 0 []
  where
    runAllTestsIter :: Int -> [Test] -> [Test] -> IO Int
    runAllTestsIter marks successful [] = return marks
    runAllTestsIter marks successful (test:tests) = do
      let b = eval (condition test) successful
      isSuccess <- if b then runTest feedback test else return False
      runAllTestsIter (if isSuccess then mark test + marks else marks)
        (if isSuccess then test:successful else successful) tests

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

{-
  The helper functions below are used to add timeouts to tests.

  They apply 'within' to the given test and also do a clever trick
  to make sure the variables are quantified outside the 'within'.
-}

test0 :: Testable prop => prop -> Property
test0 foo = within time foo

test1 :: (Arbitrary a, Show a, Testable prop) => (a -> prop) -> Property
test1 foo = property (\x -> within time $ foo x)

test2 :: (Arbitrary a, Show a, Arbitrary b, Show b, Testable prop) => (a -> b -> prop) -> Property
test2 foo = property (\x y -> within time $ foo x y)

test3 :: (Arbitrary a, Show a, Arbitrary b, Show b, Arbitrary c, Show c, Testable prop) => (a -> b -> c -> prop) -> Property
test3 foo = property (\x y z -> within time $ foo x y z)

testSized0 foo size = mapSize (\_ -> size) $ test0 foo
testSized1 foo size = mapSize (\_ -> size) $ test1 foo
testSized2 foo size = mapSize (\_ -> size) $ test2 foo
testSized3 foo size = mapSize (\_ -> size) $ test0 foo

{-
  The helper functions below are like the ones above but use an explicitly
  given test case generator and shrink function instead of the default ones.

  This is useful when you want to run a test on only a relatively small subset
  of its input type, e.g. permutations of [1..n] instead of all integer lists.

  Note that tests with more than one input are uncurried so the generator and
  shrink function must have types such as 'Gen (a, b)' and '(a, b) -> [(a, b)]'.
-}

testWith1 :: (Arbitrary a, Show a, Testable prop) => (a -> prop) -> Gen a -> (a -> [a]) -> Property
testWith1 foo gen shrink = forAllShrink gen shrink (\x -> within time $ foo x)

testWith2 :: (Arbitrary a, Show a, Arbitrary b, Show b, Testable prop) => (a -> b -> prop) -> Gen (a, b) -> ((a, b) -> [(a, b)]) -> Property
testWith2 foo gen shrink = forAllShrink gen shrink (\(x, y) -> within time $ foo x y)

testWith3 :: (Arbitrary a, Show a, Arbitrary b, Show b, Arbitrary c, Show c, Testable prop) => (a -> b -> c -> prop) -> Gen (a, b, c) -> ((a, b, c) -> [(a, b, c)]) -> Property
testWith3 foo gen shrink = forAllShrink gen shrink (\(x, y, z) -> within time $ foo x y z)

--------------------------------------------------------------------------------

tests :: [Test]
tests = [
  test_empty_trie_does_not_contain_empty_string,
  test_empty_trie_is_empty,
  test_empty_trie_contains_nothing,
  test_trie_contains_one_after_insert,
  test_contains_empty_after_insert,
  test_non_empty_after_insert,
  test_trie_contains_string_after_insert,
  test_list_to_trie_to_list,
  test_find_trie,
  test_word_rect_is_rect,
  test_all_word_rects_correct,
  test_word_rect_full,
  test_word_rect_one
  ]

readDict :: String -> IO [String]
readDict path = do
  content <- readFile path
  return (map (map toUpper) $ lines content)

common100 :: IO [String]
common100 = readDict "./common100.txt"

english2 :: IO [String]
english2 = readDict "./english2.txt"

test_empty_trie_does_not_contain_empty_string = Test {
  mark = 0,
  description = "Checking whether '\"\"' is contained in 'emptyTrie'...",
  successMsg = "'epsilon emptyTrie' correctly returned 'False'",
  failMsg = "'epsilon emptyTrie' should return 'False'",
  test = test0 prop_empty_trie_does_not_contain_empty_string,
  condition = Always
}

test_empty_trie_is_empty = Test {
  mark = 0,
  description = "Checking whether the empty Trie is empty...",
  successMsg = "'isEmpty emptyTrie' correctly returned 'True'",
  failMsg = "'isEmpty emptyTrie' should return 'True'",
  test = test0 prop_empty_trie_is_empty,
  condition = Always
}

test_empty_trie_contains_nothing = Test {
  mark = 0,
  description = "Checking whether the empty Trie contains any strings...",
  successMsg = "'trieToList emptyTrie' correctly returned '[]'",
  failMsg = "'trieToList emptyTrie' should return '[]'",
  test = test0 prop_empty_trie_contains_nothing,
  condition = Always
}

test_trie_contains_one_after_insert = Test {
  mark = 0,
  description = "Checking the empty Trie contains exactly one string after an insert...",
  successMsg = "The empty Trie did contain one string after an insert",
  failMsg = "'length (trieToList (insertTrie s emptyTrie))' should equal '1'",
  test = test1 prop_trie_contains_one_after_insert,
  condition = Always
}

test_contains_empty_after_insert = Test {
  mark = 0,
  description = "Checking the empty string is contained in a Trie after its insertion...",
  successMsg = "'epsilon (insertTrie \"\" emptyTrie)' correctly returns 'True'",
  failMsg = "'epsilon (insertTrie \"\" emptyTrie)' should return 'True'",
  test = test0 prop_contains_empty_after_insert,
  condition = Always
}

test_non_empty_after_insert = Test {
  mark = 0,
  description = "Checking a Trie is non empty after an insertion...",
  successMsg = "'isEmpty (insertTrie s emptyTrie)' correctly returned 'False'",
  failMsg = "'isEmpty (insertTrie s emptyTrie)' should return 'False'",
  test = test1 prop_non_empty_after_insert,
  condition = Always
  }

test_trie_contains_string_after_insert = Test {
  mark = 0,
  description = "Checking that inserting a string into a Trie works...",
  successMsg = "'s' is correctly an element of 'trieToList (insertTrie s emptyTrie)'",
  failMsg = "'s' should be an element of 'trieToList (insertTrie s emptyTrie)'",
  test = test1 prop_trie_contains_string_after_insert,
  condition = Always
}

test_list_to_trie_to_list = Test {
  mark = 0,
  description = "Checking that inserting a list of strings into a Trie is correct...",
  successMsg = "'trieToList (foldr insertTrie emptyTrie ss)' correctly contains the same elements as 'ss'",
  failMsg = "'trieToList (foldr insertTrie emptyTrie ss)' should contain the same elements as 'ss'",
  test = test1 prop_list_to_trie_to_list,
  condition = Always
}

test_find_trie = Test {
  mark = 0,
  description = "Checking that findTrie is correct...",
  successMsg = "'findTrie' is correct",
  failMsg = "'findTrie' does not agree with the sample solution",
  test = test2 prop_find_trie,
  condition = Always
}

test_word_rect_is_rect = Test {
  mark = 0,
  description = "Checking that 'wordRects' returns a rectangle of correct size ...",
  successMsg = "'wordRects' returned rectangles of correct size",
  failMsg = "'wordRects' did not return a 2x3 rectangles with 'common100' and a 2x2 grid",
  test = test0 (prop_word_rect_is_rect common100 2 2),
  condition = Always
}

test_all_word_rects_correct = Test {
  mark = 0,
  description = "Checking that all rectangles returned are correct...",
  successMsg = "'wordRects' returned only correct rectangles",
  failMsg = "'wordRects' returned incorrect rectangles with 'common100' and a 2x2 grid",
  test = test0 (prop_all_word_rects_correct common100 2 2),
  condition = Always
}

test_word_rect_full = Test {
  mark = 0,
  description = "Checking your 'wordRects' returns all correct word rectangles...",
  successMsg = "'wordRects' returned all correct rectangles",
  failMsg = "'wordRects' did not return all correct rectangles for 'common100' and a 2x2 grid",
  test = test0 (prop_word_rect_full common100 2 2),
  condition = Always
}

test_word_rect_one = Test {
  mark = 0,
  description = "Checking that your word rects can find a correct word rectangle...",
  successMsg = "'wordRects' found a correct word rectangle",
  failMsg = "'wordRects' did not find a correct word rectangle on 'english2' and a 3x3 grid",
  test = test0 (prop_word_rect_one english2 3 3),
  condition = Always
}
