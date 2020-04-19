module Assessed3Marking where

import Control.Monad
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.Random

import Types
import Assessed3TestCases
import qualified Assessed3Solutions as Sample

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

arbitraryLegalMoves :: Gen (Board, Cell)
arbitraryLegalMoves = do
  b <- arbitrary `suchThat` (\b -> not $ null $ Sample.legalMoves (turn b) b)
  c <- elements (Sample.legalMoves (turn b) b)
  return (b, c)

arbitraryWinningBoards :: Gen Board
arbitraryWinningBoards = do
  b <- arbitrary
  return $ b { free = [] }

arbitraryBestMovesInput = do
  b <- arbitrarySmallBoards
  sfn <- arbitrary
  return (b, sfn)

--------------------------------------------------------------------------------

tests :: [Test]
tests = [ test_legalMoves_sub
        , test_legalMoves_sub_partial
        , test_legalMoves_sup
        , test_legalMoves_sup_partial
        , test_moveLegal_turn
        , test_moveLegal_free_sub
        , test_moveLegal_free_sup
        , test_moveLegal_hist
        , test_moveLegal_hist_partial
        , test_replay_empty
        , test_replay_last
        , test_replay_last_partial
        , test_replay_rest
        , test_replay_rest_partial_star
        , test_replay_rest_partial_free
        , test_replay_rest_partial_hist
        , test_replay_rest_partial_turn
        , test_score_win
        , test_score_heu
        , test_score_heu_partial
        , test_minimax_empty
        , test_minimax_fmap
        , test_minimax_H
        , test_minimax_V_success
        , test_minimax_V_fail
        , test_bestmoves_sub
        , test_bestmoves_sub_partial
        , test_bestmoves_sup
        , test_bestmoves_pruning
        , test_runGame_id_H
        , test_runGame_id_V
        , test_runGame_illegal_move_H
        , test_runGame_illegal_move_V
        , test_runGame_H
        , test_runGame_V
        , test_runGame_list
        , test_carpets_zero
        , test_carpets_hist
        , test_carpets_turn_H
        , test_carpets_turn_V
        , test_carpets_free
        , test_carpets_free_partial_length
        , test_carpets_free_partial_nub
        ]


-- Test for legalMoves

test_legalMoves_sub = Test {
  mark = 3,
  description = newSection ++ "Checking if all moves returned by 'legalMoves' are legal...",
  successMsg = "You got 3 marks as all moves returned by 'legalMoves' were legal",
  failMsg = "'legalMoves' returned non legal moves, checking for partial marks...",
  test = test2 prop_legalMoves_sub,
  condition = Always
}

test_legalMoves_sub_partial = Test {
  mark = 2,
  description = "Checking if all moves returned by 'legalMoves' are legal when the input player is next to play...",
  successMsg = "You got 2 marks as all moves returned by 'legalMoves' were legal when the input player was next to play. You likely used the player from the board instead of the input player.",
  failMsg = "'legalMoves' returned non legal maves when the input player was next to play.",
  test = test1 prop_legalMoves_sub_partial,
  condition = IfFail test_legalMoves_sub
}

test_legalMoves_sup = Test {
  mark = 2,
  description = "Checking if all legal moves are returned by 'legalMoves'...",
  successMsg = "You got 2 marks as 'legalMoves' returns all legal moves.",
  failMsg = "Not all legal moves were returned by 'legalMoves', checking for partial marks...",
  test = test2 prop_legalMoves_sup,
  condition = Always
}

test_legalMoves_sup_partial = Test {
  mark = 1,
  description = "Checking if all legal moves are returned by 'legalMoves' when the input player is next to play...",
  successMsg = "You got 1 mark as all legal moves were returned by 'legalMoves' when the input player was next to play. You likely used the player from the board instead of the input player.",
  failMsg = "Not all legal moves were returned by 'legalMoves' when the input player was next to play.",
  test = test1 prop_legalMoves_sup_partial,
  condition = IfFail test_legalMoves_sup
}

-- Tests for moveLegal

test_moveLegal_turn = Test {
  mark = 1,
  description = newSection ++ "Checking that 'moveLegal' flips the current player...",
  successMsg = "You got 1 mark as 'moveLegal' acts correctly on the turn.",
  failMsg = "'moveLegal' did not correctly flip the turn.",
  test = testWith2 prop_moveLegal_turn arbitraryLegalMoves shrinkNothing,
  condition = Always
}

test_moveLegal_free_sub = Test {
  mark = 1,
  description = "Checking that the board resulting from 'moveLegal' does not contain extra free squares...",
  successMsg = "You got 1 mark as the board resulting from 'moveLegal' does not contain extra free squares.",
  failMsg = "The board resulting from 'moveLegal' has extra free squares.",
  test = testWith2 prop_moveLegal_free_sub arbitraryLegalMoves shrinkNothing,
  condition = Always
}

test_moveLegal_free_sup = Test {
  mark = 1,
  description = "Checking that 'moveLegal' only fills in squares that it should...",
  successMsg = "You got 1 mark as 'moveLegal' only fills in squares that it should.",
  failMsg = "Your 'moveLegal' fills in squares that it shouldn't fill.",
  test = testWith2 prop_moveLegal_free_sup arbitraryLegalMoves shrinkNothing,
  condition = Always
}

test_moveLegal_hist = Test {
  mark = 2,
  description = "Checking that 'moveLegal' correctly modifies the history...",
  successMsg = "You got 2 marks as your 'moveLegal' acts correctly on the history.",
  failMsg = "Your 'moveLegal' did not correctly modify the history, checking for partial marks...",
  test = testWith2 prop_moveLegal_hist arbitraryLegalMoves shrinkNothing,
  condition = Always
}

test_moveLegal_hist_partial = Test {
  mark = 1,
  description = "Checking that your 'moveLegal' adds the correct element to the history...",
  successMsg = "You got 1 mark as 'history $ moveLegal b m' is a permutation of the correct answer.",
  failMsg = "The board returned by your 'moveLegal' had incorrect elements in its history.",
  test = testWith2 prop_moveLegal_hist_partial arbitraryLegalMoves shrinkNothing,
  condition = IfFail test_moveLegal_hist
}

 -- Tests for replay
test_replay_empty = Test {
  mark = 1,
  description = newSection ++ "Checking that replay is correct on a board with no history...",
  successMsg = "You got 1 mark as your 'replay' was correct on a board with no history.",
  failMsg = "'replay b' should equal 'b' when 'hist b == []'",
  test = test1 prop_replay_empty,
  condition = Always
}

test_replay_last = Test {
  mark = 2,
  description = "Checking that the last element of 'replay b' is 'b'...",
  successMsg = "You got 2 marks as the last element of 'replay b' was 'b'.",
  failMsg = "The last element of 'replay b' should be 'b', checking for partial marks...",
  test = test1 prop_replay_last,
  condition = Always
}

test_replay_last_partial = Test {
  mark = 1,
  description = "Checking 'b' occurs in 'replay b'...",
  successMsg = "You got 1 mark as 'b' occurs in 'replay b'.",
  failMsg = "'b' did not occur in 'replay b'.",
  test = test1 prop_replay_last_partial,
  condition = IfFail test_replay_last
}

test_replay_rest = Test {
  mark = 7,
  description = "Checking that 'replay' is correct...",
  successMsg = "You got 7 marks as 'replay' is correct (up to the original (input) board).",
  failMsg = "'replay' was not correct, checking for partial marks...",
  test = test1 prop_replay_rest,
  condition = Always
}

test_replay_rest_partial_star = Test {
  mark = 5,
  description = "Checking that 'replay' contains the correct elements...",
  successMsg = "You got 5 marks as 'replay' returns the correct elements (but not necessarily in the right order).",
  failMsg = "Your 'replay' does not contain the right elements, checking for partial marks...",
  test = test1 prop_replay_rest_partial_star,
  condition = IfFail test_replay_rest
}

test_replay_rest_partial_free = Test {
  mark = 2,
  description = "Checking that 'map free $ replay b' is correct...",
  successMsg = "You got 2 marks as the 'free' component of 'replay b' was correct.",
  failMsg = "The 'free' component of 'replay b' was not correct.",
  test = test1 prop_replay_rest_partial_free,
  condition = All [IfFail test_replay_rest, IfFail test_replay_rest_partial_star]
}

test_replay_rest_partial_hist = Test {
  mark = 2,
  description = "Checking that 'map hist $ replay b' is correct...",
  successMsg = "You got 2 marks as the 'hist' component of 'replay b' was correct.",
  failMsg = "The 'hist' component of 'replay b' was not correct.",
  test = test1 prop_replay_rest_partial_hist,
  condition = All [IfFail test_replay_rest, IfFail test_replay_rest_partial_star]
}

test_replay_rest_partial_turn = Test {
  mark = 1,
  description = "Checking that 'map turn $ replay b' is correct...",
  successMsg = "You got 1 mark as the 'turn' component of 'replay b' was correct.",
  failMsg = "The 'turn' component of 'replay b' was not correct.",
  test = test1 prop_replay_rest_partial_turn,
  condition = All [IfFail test_replay_rest, IfFail test_replay_rest_partial_star]
}

-- Tests for score

test_score_win = Test {
  mark = 2,
  description = newSection ++ "Checking that 'score' is correct when there is a winner...",
  successMsg = "You got 2 marks as 'score' is correct when there is a winner.",
  failMsg = "'score' was not correct when there is a winner.",
  test = testWith1 prop_score_win arbitraryWinningBoards shrinkNothing,
  condition = Always
}

test_score_heu = Test {
  mark = 3,
  description = "Checking that 'score' is correct when there is no winner...",
  successMsg = "You got 3 marks as 'score' is correct when there is no winner.",
  failMsg = "'score' was not correct when there is no winner, checking for partial marks...",
  test = test1 prop_score_heu,
  condition = Always
}

test_score_heu_partial = Test {
  mark = 1,
  description = "Checking that your 'score' returns a heuristic on boards with no winner...",
  successMsg = "You got 1 mark as your 'score' returns a heuristic on boards with no winner.",
  failMsg = "Your 'score' does not always return a heuristic on boards with no winner.",
  test = test1 prop_score_heu_partial,
  condition = IfFail test_score_heu
}

-- Tests for minimax

test_minimax_empty = Test {
  mark = 1,
  description = newSection ++ "Checking that 'minimax' is correct on leaves...",
  successMsg = "You got 1 mark as minimax was correct on leaves.",
  failMsg = "'minimax' was not correct on leaves.",
  test = test2 prop_minimax_empty,
  condition = Always
}

test_minimax_fmap = Test {
  mark = 1,
  description = "Checking that 'minimax' does not modify the tree structure...",
  successMsg = "You got 1 mark as 'minimax' does not modify the tree structure.",
  failMsg = "Your 'minimax' modified the tree structure.",
  test = test2 prop_minimax_fmap,
  condition = Always
}

test_minimax_H = Test {
  mark = 1,
  description = "Checking the recursive cases for 'minimax'...",
  successMsg = "",
  failMsg = "",
  test = test2 prop_minimax_H,
  condition = Always
}

test_minimax_V_success = Test {
  mark = 2,
  description = "",
  successMsg = "You got 3 marks as both recursive cases were correct for 'minimax'.",
  failMsg = "You got 1 mark as the 'H' recursive case was correct but the 'V' case was not.",
  test = test2 prop_minimax_V,
  condition = IfSuccess test_minimax_H
}

test_minimax_V_fail = Test {
  mark = 1,
  description = "",
  successMsg = "You got 1 mark as the 'V' recursive case was correct but the 'H' case was not.",
  failMsg = "The recursive cases were not correct for 'minimax'.",
  test = test2 prop_minimax_V,
  condition = IfFail test_minimax_H
}

-- Tests for bestmoves

test_bestmoves_sub = Test {
  mark = 4,
  description = newSection ++ "Checking that all moves returned by 'bestmoves' are optimal...",
  successMsg = "You got 4 marks as all moves returned by 'bestmoves' were optimal.",
  failMsg = "Your 'bestmoves' returns non optimal moves.",
  test = testWith2 prop_bestmoves_sub arbitraryBestMovesInput shrinkNothing,
  condition = Always
}

test_bestmoves_sub_partial = Test {
  mark = 1,
  description = "Checking that all moves returned by 'bestmoves' are legal...",
  successMsg = "You got 1 mark as all moves returned by 'bestmoves' were legal.",
  failMsg = "'bestmoves' returned illegal moves.",
  test = testWith2 prop_bestmoves_sub_partial arbitraryBestMovesInput shrinkNothing,
  condition = IfFail test_bestmoves_sub
}

test_bestmoves_sup = Test {
  mark = 4,
  description = "Checking that 'bestmoves' returns all optimal moves...",
  successMsg = "You got 4 marks as 'bestmoves' returns all optimal moves.",
  failMsg = "'bestmoves' did not return all optimal moves.",
  test = testWith2 prop_bestmoves_sup arbitraryBestMovesInput shrinkNothing,
  condition = Always
}

test_bestmoves_pruning = Test {
  mark = 2,
  description = "Checking that 'bestmoves' does not timeout on a large board...",
  successMsg = "You got 2 marks as 'bestmoves' did not timeout on a large board.",
  failMsg = "'bestmoves' timed out on a large board so is likely not pruning correctly or maybe your 'legalMoves' is too slow.",
  test = test0 prop_bestmoves_pruning,
  condition = Always
}

-- Tests for rungame

test_runGame_id_H = Test {
  mark = 1,
  description = newSection ++ "Checking that 'runGame' handles player 'H' returning 'Nothing' correctly...",
  successMsg = "You got 1 mark as 'runGame' returns an unchanged board when player 'H' returns 'Nothing'.",
  failMsg = "'runGame' does not handle player 'H' returning 'Nothing' correctly.",
  test = test1 prop_runGame_id_H,
  condition = Always
}

test_runGame_id_V = Test {
  mark = 1,
  description = "Checking that 'runGame' handles player 'V' returning 'Nothing' correctly...",
  successMsg = "You got 1 mark as 'runGame' returns an unchanged board when player 'V' returns 'Nothing'.",
  failMsg = "'runGame' does not handle player 'V' returning 'Nothing' correctly.",
  test = test1 prop_runGame_id_V,
  condition = Always
}

test_runGame_illegal_move_H = Test {
  mark = 1,
  description = "Checking that 'runGame' handles player 'H' returning an illegal move correctly...",
  successMsg = "You got 1 mark as 'runGame' returns an unchanged board when player 'H' returns an illegal move.",
  failMsg = "'runGame' does not handle player 'H' returning an illegal move correctly.",
  test = test2 prop_runGame_illegal_move_H,
  condition = Always
}

test_runGame_illegal_move_V = Test {
  mark = 1,
  description = "Checking that 'runGame' handles player 'V' returning an illegal move corrrectly...",
  successMsg = "You got 1 mark as 'runGame' returns an unchanged board when player 'V' returns an illegal move.",
  failMsg = "'runGame' does not handle player 'V' returning an illegal move correctly.",
  test = test2 prop_runGame_illegal_move_V,
  condition = Always
}

test_runGame_H = Test {
  mark = 2,
  description = "Checking that 'runGame' correctly runs 'playH'...",
  successMsg = "You got 2 marks as 'runGame' correctly runs 'playH'.",
  failMsg = "'runGame' did not correctly run 'playH'.",
  test = test1 prop_runGame_H,
  condition = Always
}

test_runGame_V = Test {
  mark = 2,
  description = "Checking that 'runGame' correctly runs 'playV'...",
  successMsg = "You got 2 marks as 'runGame' correctly runs 'playV'",
  failMsg = "'runGame' did not correctly run 'playV'.",
  test = test1 prop_runGame_V,
  condition = Always
}

test_runGame_list = Test {
  mark = 2,
  description = "Checking 'runGame' with the list monad...",
  successMsg = "You got 2 marks as 'runGame' was correct with the list monad.",
  failMsg = "'runGame' was not correct with the list monad.",
  test = test1 prop_runGame_list,
  condition = Always
}

-- Tests for carpets
test_carpets_zero = Test {
  mark = 1,
  description = newSection ++ "Checking 'carpets !! 0'...",
  successMsg = "You got 1 mark as 'carpets !! 0' has the correct free spaces.",
  failMsg = "'carpets !! 0' did not have the correct free spaces.",
  test = test0 prop_carpets_zero,
  condition = Always
}

test_carpets_hist = Test {
  mark = 1,
  description = "Checking 'carpets !! n' has an empty history...",
  successMsg = "You got 1 mark as 'carpets !! n' had an empty history.",
  failMsg = "'carpets !! n' did not have an empty history.",
  test = test1 prop_carpets_hist,
  condition = Always
}

test_carpets_turn_H = Test {
  mark = 1,
  description = "Checking 'turn $ carpets !! 2 * n == H'...",
  successMsg = "You got 1 mark as 'turn $ carpets !! 2 * n == H'.",
  failMsg = "'turn $ carpets !! 2 * n' should equal 'H'.",
  test = test1 prop_carpets_turn_H,
  condition = Always
}

test_carpets_turn_V = Test {
  mark = 1,
  description = "Checking 'turn $ carpets !! 2 * n + 1 == V'...",
  successMsg = "You got 1 mark as 'turn $ carpets !! 2 * n + 1 == V'.",
  failMsg = "'turn $ carpets !! 2 * n + 1' should equal 'V'.",
  test = test1 prop_carpets_turn_V,
  condition = Always
}

test_carpets_free = Test {
  mark = 6,
  description = "Checking 'carpets !! n' has the correct free spaces...",
  successMsg = "You got 6 marks as 'carpets !! n' has the correct free spaces.",
  failMsg = "'carpets !! n' does not have the correct free spaces, checking for partial marks...",
  test = test1 prop_carpets_free,
  condition = Always
}

test_carpets_free_partial_length = Test {
  mark = 1,
  description = "Checking 'carpets !! n' has the correct number of free spaces...",
  successMsg = "You got 1 mark as 'carpets !! n' has the correct number of free spaces.",
  failMsg = "'carpets !! n' did not have the correct number of free spaces.",
  test = test1 prop_carpets_free_partial_length,
  condition = IfFail test_carpets_free
}

test_carpets_free_partial_nub = Test {
  mark = 2,
  description = "Checking 'carpets !! n' has the correct free spaces when ignoring duplicates...",
  successMsg = "You got 2 marks as 'carpets !1 n' has the correct number of free spaces when ignoring duplicates.",
  failMsg = "'carpets !! n' did not have the correct free spaces, even when ignoring duplicates.",
  test = test1 prop_carpets_free_partial_nub,
  condition = IfFail test_carpets_free
}
