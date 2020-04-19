module Assessed2Marking where

import Control.Monad
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.Random

import Assessed2TestCases

-- Test timeout in microseconds.
time :: Int
time = 1000000

-- Whether the assignment is assesed.
isAssessed :: Bool
isAssessed = True

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
       , maxShrinks = if feedback then 30 else 0
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
tests = [ test_showRank_correctness
        , test_showRank_2to10
        , test_showRank_JtoA
        , test_showSuit_correctness
        , test_showSuit_missing_backslash
        , test_showCard_correctness
        , test_showCard_red
        , test_showCard_black
        , test_showCard_rank_sub
        , test_showCard_suit_sub
        , test_choose_io
        , test_choose_list_sub
        , test_choose_list_sup
        , test_choose_dist
        , test_simulate_io
        , test_simulate_id_true
        , test_simulate_id_false
        , test_simulate_list
        , test_simulate_dist
        , test_cut_id
        , test_cut_io
        , test_cut_list_sub
        , test_cut_list_sup
        , test_cut_dist
        , test_shuffle_id_l
        , test_shuffle_id_r
        , test_shuffle_id_both
        , test_shuffle_io
        , test_shuffle_list_sub
        , test_shuffle_list_sup
        , test_shuffle_dist
        , test_riffles_id_empty
        , test_riffles_id_0
        , test_riffles_io
        , test_riffles_dist
        , test_riffles_num_calls_cut
        , test_riffles_num_calls_shuffle
        , test_permute_id
        , test_permute_io
        , test_permute_list
        , test_permute_list_sup
        , test_permute_dist
        , test_genTree_id
        , test_genTree_io
        , test_genTree_list
        , test_genTree_list_sup
        , test_genTree_dist
        ]

-- Tests for show Rank.
test_showRank_correctness = Test {
  mark = 5,
  description = newSection ++ "Checking if 'show rank' is correct...",
  successMsg = "'show rank' is correct.",
  failMsg = "'show rank' is not correct, checking for partial marks...",
  test = test1 prop_showRank_correctness,
  condition = Always
}

test_showRank_2to10 = Test {
  mark = 2,
  description = "Checking if you show ranks 2 to 10 correctly...",
  successMsg = "You got 2 marks for showing ranks 2 to 10 correctly.",
  failMsg = "One of the ranks in 2 to 10 is not shown correctly.",
  test = test1 prop_showRank_2to10,
  condition = IfFail test_showRank_correctness
}

test_showRank_JtoA = Test {
  mark = 2,
  description = "Checking if you show ranks J,Q,K and A correctly...",
  successMsg = "You got 2 marks for showing ranks J,Q,K and A correctly.",
  failMsg = "One of the ranks J,Q,K or A is not shown correctly.",
  test = test1 prop_showRank_JtoA,
  condition = IfFail test_showRank_correctness
}

-- Tests for show Suit.
test_showSuit_correctness = Test {
  mark = 5,
  description = newSection ++ "Checking if 'show suit' is correct...",
  successMsg = "'show suit' is correct.",
  failMsg = "'show suit' is not correct, checking for partial marks...",
  test = test1 prop_showSuit_correctness,
  condition = Always
}

test_showSuit_missing_backslash = Test {
  mark = 2,
  description = "Checking if you have the correct Unicode Decimal Codes...",
  successMsg = "You got 2 marks for having the correct Unicode Decimal Codes.",
  failMsg = "You did not have the correct Unicode Decimal Codes.",
  test = test1 prop_showSuit_missing_backslash,
  condition = IfFail test_showSuit_correctness
}

-- Tests for show Card.
test_showCard_correctness = Test {
  mark = 10,
  description = newSection ++ "Checking if 'show card' is correct...",
  successMsg = "'show card' is correct.",
  failMsg = "'show card' is not correct, checking for partial marks...",
  test = test1 prop_showCard_correctness,
  condition = Always
}

test_showCard_red = Test {
  mark = 2,
  description = "Checking if you show all red cards correctly...",
  successMsg = "You got 2 marks for showing all red cards correctly.",
  failMsg = "Some red cards were not shown correctly.",
  test = test1 prop_showCard_red,
  condition = IfFail test_showCard_correctness
}

test_showCard_black = Test {
  mark = 2,
  description = "Checking if you show all black cards correctly...",
  successMsg = "You got 2 marks for showing all blacks cards correctly.",
  failMsg = "Some black cards were not shown correctly.",
  test = test1 prop_showCard_black,
  condition = IfFail test_showCard_correctness
}

test_showCard_rank_sub = Test {
  mark = 2,
  description = "Checking if you show the rank of the cards...",
  successMsg = "You got 2 marks for showing the rank of the cards.",
  failMsg = "You did not properly show the rank of some cards.",
  test = test1 prop_showCard_rank_sub,
  condition = IfFail test_showCard_correctness
}

test_showCard_suit_sub = Test {
  mark = 2,
  description = "Checking if you show the suit of the cards...",
  successMsg = "You got 2 marks for showing the suit of the cards.",
  failMsg = "You did not properly show the suit of some cards.",
  test = test1 prop_showCard_suit_sub,
  condition = IfFail test_showCard_correctness
}

-- Tests for choose.
test_choose_io = Test {
  mark = 1,
  description = newSection ++ "Checking if 'choose' is correct for the IO monad (and large values)...",
  successMsg = "You got 1 mark for having a correct 'choose' for the IO monad.",
  failMsg = "'choose' is not correct for the IO monad.",
  test = testSized1 prop_choose_io 10000,
  condition = Always
}

test_choose_list_sub = Test {
  mark = 1,
  description = "Checking if 'choose' is correct for the List monad (1/2) by checking if choose returns a subset of its input...",
  successMsg = "You got 1 mark for having 'choose' on the List monad return a subset of its input.",
  failMsg = "On the List monad, 'choose' should return a subset of its input.",
  test = test1 prop_choose_list_sub,
  condition = Always
}

test_choose_list_sup = Test {
  mark = 1,
  description = "Checking if 'choose' is correct for the List monad (2/2) by checking if 'choose' returns a superset of its input...",
  successMsg = "You got 1 mark for having 'choose' on the List return a superset of its input.",
  failMsg = "On the List monad, 'choose' should return a superset of its input.",
  test = test1 prop_choose_list_sup,
  condition = Always
}

test_choose_dist = Test {
  mark = 2,
  description = "Checking if 'choose' is correct for the Dist monad...",
  successMsg = "You got 2 marks for having a correct 'choose' for the Dist monad. ",
  failMsg = "'choose' is not correct for the Dist monad.",
  test = test1 prop_choose_dist,
  condition = Always
}

-- Tests for simulate.
test_simulate_io = Test {
  mark = 1,
  description = newSection ++ "Checking if 'simulate' is correct for the IO monad (and large values)...",
  successMsg = "You got 1 mark for having a correct 'simulate' for the IO monad.",
  failMsg = "'simulate' is not correct for the IO monad.",
  test = testSized2 prop_simulate_io 10000,
  condition = Always
}

test_simulate_id_true = Test {
  mark = 1,
  description = "Checking if 'simulate' is correct for the Identity monad (1/2) by checking it on the experiment that is always true...",
  successMsg = "You got 1 mark for having 'simulate k True = k' on the Identity monad.",
  failMsg = "On the Identity monad, 'simulate k True' should be 'k'.",
  test = test1 prop_simulate_id_true,
  condition = Always
}

test_simulate_id_false = Test {
  mark = 1,
  description = "Checking if 'simulate' is correct for the Identity monad (2/2) by checking it on the experiment that is always false...",
  successMsg = "You got 1 mark for having 'simulate k False = 0' on the Identity monad.",
  failMsg = "On the Identity monad, 'simulate k False' should be '0'.",
  test = test1 prop_simulate_id_false,
  condition = Always
}

test_simulate_list = Test {
  mark = 1,
  description = "Checking if 'simulate' is correct for the List monad...",
  successMsg = "You got 1 mark for having a correct 'simulate' on the List monad.",
  failMsg = "'simulate' is not correct for the List monad.",
  test = test1 prop_simulate_list,
  condition = Always
}

test_simulate_dist = Test {
  mark = 1,
  description = "Checking if 'simulate' is correct for the Dist monad...",
  successMsg = "You got 1 mark for having a correct 'simulate' on the Dist monad.",
  failMsg = "'simulate' is not correct for the Dist monad.",
  test = test1 prop_simulate_dist,
  condition = Always
}

-- Q3
-- Tests for cut.
test_cut_id = Test {
  mark = 1,
  description = newSection ++ "Checking 'cut' with the Identity monad on the empty list...",
  successMsg = "You got 1 mark as your 'cut [] = ([],[])' with the Identity monad",
  failMsg = "'cut []' should equal '([],[])' with the Identity monad.",
  test = test0 prop_cut_id,
  condition = Always
}

test_cut_io = Test {
  mark = 1,
  description = "Checking 'cut' with the IO monad (and large values)...",
  successMsg = "You got 1 mark as your 'cut' function works with the IO monad.",
  failMsg = "The output of 'cut' should concatenate to give the input to cut.",
  test = testSized1 prop_cut_io 10000,
  condition = Always
}

test_cut_list_sub = Test {
  mark = 1,
  description = "Checking that 'cut' is correct for the List monad (1/2) by checking that all returned cuts are valid...",
  successMsg = "You got 1 mark for having 'cut' return only cuts that are valid with the List monad.",
  failMsg = "Your 'cut' returns items not in the correct solution with the List monad.",
  test = test1 prop_cut_list_sub,
  condition = Always
}

test_cut_list_sup = Test {
  mark = 1,
  description = "Checking that 'cut' is correct for the List monad (2/2) by checking that all valid cuts are returned...",
  successMsg = "You got 1 mark for having 'cut' return all valid cuts with the list monad.",
  failMsg = "The output of your 'cut' does not contain all valid cuts.",
  test = test1 prop_cut_list_sup,
  condition = Always
}

test_cut_dist = Test {
  mark = 1,
  description = "Checking 'cut' with the Dist monad",
  successMsg = "You got 1 mark as your 'cut' functions correctly with the Dist monad.",
  failMsg = "Your 'cut' did not return the correct distibution with the Dist monad.",
  test = test1 prop_cut_dist,
  condition = Always
}

-- Tests for shuffle.
test_shuffle_id_l = Test {
  mark = 1,
  description = newSection ++ "Checking 'shuffle' with the Identity monad (1/3) by checking the input '([],xs)'...",
  successMsg = "You got 1 mark for having 'shuffle ([],xs) = xs' with the Identity monad.",
  failMsg = "'shuffle ([],xs)' should equal 'xs' with the Identity monad.",
  test = test1 prop_shuffle_id_l,
  condition = Always
}

test_shuffle_id_r = Test {
  mark = 1,
  description = "Checking 'shuffle' with the Identity monad (2/3) by checking the input '(xs,[])'...",
  successMsg = "You got 1 mark for having 'shuffle (xs,[]) = xs' with the Identity monad.",
  failMsg = "'shuffle (xs,[])' should equal 'xs' with the Identity monad.",
  test = test1 prop_shuffle_id_r,
  condition = Always
}

test_shuffle_id_both = Test {
  mark = 1,
  description = "Checking 'shuffle' with the Identity monad (3/3) by checking the input '([],[])'...",
  successMsg = "You got 1 mark for having 'shuffle ([],[]) = []' with the Identity monad.",
  failMsg = "'shuffle ([],[])' should equal '[]' with the Identity monad.",
  test = test0 prop_shuffle_id_both,
  condition = Always
}

test_shuffle_io = Test {
  mark = 1,
  description = "Checking 'shuffle' with the IO monad (and large values)...",
  successMsg = "You got 1 mark as 'shuffle' with IO monad returns a valid interleaving.",
  failMsg = "Your 'shuffle' does not return a valid interleaving with the IO monad.",
  test = testSized1 prop_shuffle_io 1000,
  condition = Always
}

test_shuffle_list_sub = Test {
  mark = 1,
  description = "Checking 'shuffle' with the List monad (1/2) by checking all returned lists are valid interleavings...",
  successMsg = "You got 1 mark for having 'shuffle' only return valid interleavings with the List monad.",
  failMsg = "Your 'shuffle' returns lists which are not valid interleavings when ran with the List monad.",
  test = testSized1 prop_shuffle_list_sub 3,
  condition = Always
}

test_shuffle_list_sup = Test {
  mark = 1,
  description = "Checking 'shuffle' with the List monad (2/2) by checking all valid interleavings are returned...",
  successMsg = "You got 1 mark for having 'shuffle' return all valid interleavings with the List monad.",
  failMsg = "Your 'shuffle' does not return all valid interleavings When ran with the List monad.",
  test = testSized1 prop_shuffle_list_sup 3,
  condition = Always
}

test_shuffle_dist = Test {
  mark = 4,
  description = "Checking 'shuffle' with the Dist monad...",
  successMsg = "You got 4 marks as your 'shuffle' returns the correct distribution.",
  failMsg = "Your 'shuffle' does not return the correct distribution.",
  test = testSized1 prop_shuffle_dist 3,
  condition = Always
}

-- Tests for riffles.
test_riffles_id_empty = Test {
  mark = 1,
  description = newSection ++ "Checking 'riffles' with the Identity monad (1/2), by checking it on the empty list...",
  successMsg = "You got 1 mark for having 'riffles cf sf n [] = []' with the Identity monad.",
  failMsg = "'riffles cf sf n []' should equal '[]' with the Identity monad.",
  test = test1 prop_riffles_id_empty,
  condition = Always
}

test_riffles_id_0 = Test {
  mark = 1,
  description = "Checking 'riffles' with the Identity monad (2/2), by checking it with 'n = 0'...",
  successMsg = "You got 1 mark for having 'riffles cf sf 0 xs = xs' with the Identity monad.",
  failMsg = "'riffles cf sf 0 xs' should equal 'xs' with the Identity monad.",
  test = test1 prop_riffles_id_0,
  condition = Always
}

test_riffles_io = Test {
  mark = 1,
  description = "Checking 'riffles' with the IO monad (and large inputs)...",
  successMsg = "You got 1 mark as your 'riffles' returns a permutation of its input list with the IO monad.",
  failMsg = "'riffles' should always return a permutation of its input list with the IO monad.",
  test = testSized2 prop_riffles_io 100,
  condition = Always
}

test_riffles_dist = Test {
  mark = 1,
  description = "Checking 'riffles' with the Dist monad...",
  successMsg = "You got 1 mark as your 'riffles' returns the correct distribution.",
  failMsg = "Your 'riffles' does not return the correct distribution.",
  test = testSized2 prop_riffles_dist 3,
  condition = Always
}

test_riffles_num_calls_cut = Test {
  mark = 0,
  description = "Checking 'riffles' calls 'cut' and 'shuffle' the correct number of times...",
  successMsg = "",
  failMsg = "'riffles cf sf n xs' should call 'cut' 'n' times.",
  test = test2 prop_riffles_num_calls_cut,
  condition = Always
}

test_riffles_num_calls_shuffle = Test {
  mark = 1,
  description = "",
  successMsg = "You got 1 mark as your 'riffles' calls 'cut' and 'shuffle' the correct number of times.",
  failMsg = "'riffles cf sf n xs' should call 'shuffle' 'n' times.",
  test = test2 prop_riffles_num_calls_shuffle,
  condition = IfSuccess test_riffles_num_calls_cut
}

-- Tests for permute.
test_permute_id = Test {
  mark = 1,
  description = newSection ++ "Checking 'permute' with the Identity monad, by checking it on the empty list...",
  successMsg = "You got 1 mark for having 'permute [] = []' with the Identity monad.",
  failMsg = "'permute []' should equal '[]' with the Identity monad.",
  test = test0 prop_permute_id,
  condition = Always
}

test_permute_io = Test {
  mark = 1,
  description = "Checking 'permute' with the IO monad (and large values)...",
  successMsg = "You got 1 mark as your 'permute' returns a permutation of its input with the IO monad.",
  failMsg = "'permute' should return a permutation of its input with the IO monad.",
  test = testSized1 prop_permute_io 1000,
  condition = Always
}

test_permute_list = Test {
  mark = 2,
  description = "Checking 'permute' with the List monad...",
  successMsg = "You got 2 marks as your 'permute' is correct with the List monad.",
  failMsg = "Your 'permute' was not correct with the List monad. Checking for partial marks...",
  test = testSized1 prop_permute_list 6,
  condition = Always
}

test_permute_list_sup = Test {
  mark = 1,
  description = "Checking 'permute' returns all permutations of its input...",
  successMsg = "You got 1 mark for having 'permute' return all permutations of its input with the List monad.",
  failMsg = "'permute' should return all possible permutations with the List monad.",
  test = testSized1 prop_permute_list_sup 6,
  condition = IfFail test_permute_list
}

test_permute_dist = Test {
  mark = 1,
  description = "Checking 'permute' with the Dist monad...",
  successMsg = "You got 1 mark as your 'permute' is correct with the Dist monad.",
  failMsg = "Your 'permute' does not return the correct distribution.",
  test = testSized1 prop_permute_dist 6,
  condition = Always
}

-- Tests for genTree.
test_genTree_id = Test {
  mark = 1,
  description = newSection ++ "Checking 'genTree' with the Identity monad, by checking it on the singleton list...",
  successMsg = "You got 1 mark for having 'genTree [x] = L x' with the Identity monad.",
  failMsg = "'genTree [x]' should equal 'L x' with the Identity monad.",
  test = test1 prop_genTree_id,
  condition = Always
}

test_genTree_io = Test {
  mark = 1,
  description = "Checking 'genTree' with the IO monad (and large values)...",
  successMsg = "You got 1 mark as your 'genTree' produces a valid tree with the IO monad.",
  failMsg = "'genTree xs' should return a tree whose canopy is a permutation of 'xs' with the IO monad.",
  test = testSized1 prop_genTree_io 100,
  condition = Always
}

test_genTree_list = Test {
  mark = 2,
  description = "Checking 'genTree' with the List monad",
  successMsg = "You got 2 marks as your 'genTree' is correct with the List monad.",
  failMsg = "Your 'genTree' was not correct with the List monad. Checking for partial marks...",
  test = testSized1 prop_genTree_list 4,
  condition = Always
}

test_genTree_list_sup = Test {
  mark = 1,
  description = "Checking 'genTree' returns all correct trees...",
  successMsg = "You got 1 mark for having 'genTree' return all possible correct trees with the List monad.",
  failMsg = "'genTree' should return all correct trees with the list monad.",
  test = testSized1 prop_genTree_list_sup 4,
  condition = IfFail test_genTree_list
}

test_genTree_dist = Test {
  mark = 1,
  description = "Checking 'genTree' with the Dist monad",
  successMsg = "You got 1 mark as 'genTree' was correct with the Dist monad.",
  failMsg = "Your 'genTree' did not return the correct distribution.",
  test = testSized1 prop_genTree_dist 4,
  condition = Always
}
