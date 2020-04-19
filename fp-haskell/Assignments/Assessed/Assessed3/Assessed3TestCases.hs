{-# LANGUAGE FlexibleInstances #-}

module Assessed3TestCases where

import Data.Tree
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.DeepSeq
import Control.Monad.Identity
import Test.QuickCheck.Gen.Unsafe

import Types
import qualified Assessed3Solutions as Sample
import qualified Assessed3 as Student

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

permOfBy :: (Eq a, Ord a) => (a -> a -> Bool) -> [a] -> [a] -> Bool
permOfBy eq xs ys = case (sort xs, sort ys) of
  ([], []) -> True
  (x:xs, y:ys) -> x `eq` y && permOfBy eq xs ys
  (_, _) -> False

p_permOfBy :: (Eq a, Ord a, Show a, NFData a) => (a -> a -> Bool) -> [a] -> [a] -> Property
p_permOfBy eq xs ys =
  counterexample ("Your output " ++ show xs ++ " is not a permutation of " ++ show ys ++ ".")
  (permOfBy eq (deepseq xs xs) (deepseq ys ys))

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

p_seteq :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
p_seteq xs ys = counterexample ("Your output " ++ show xs ++ " is not the same set as " ++ show ys ++ ".")
                (xs `deepseq` ys `deepseq` (ys `subset` xs && xs `subset` ys))

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

eqboard :: Board -> Board -> Bool
eqboard b1 b2 = turn b1 == turn b2 && hist b1 == hist b2 && free b1 `permOf` free b2

p_eqboard :: Board -> Board -> Property
p_eqboard b1 b2 = counterexample ("Your output " ++ show b1 ++ " is not the same as " ++ show b2 ++ ".")
                  (deepseq b1 b1 `eqboard` deepseq b2 b2)

eqboardlist :: [Board] -> [Board] -> Bool
eqboardlist [] [] = True
eqboardlist (x:b1) (y:b2) = x `eqboard` y && b1 `eqboardlist` b2
eqboardlist _ _ = False

p_eqboardlist :: [Board] -> [Board] -> Property
p_eqboardlist b1 b2 = counterexample ("Your output " ++ show b1 ++ " is not the same as " ++ show b2 ++ ".")
                      (deepseq b1 b1 `eqboardlist` deepseq b2 b2)
------------------------------------------------------------------------------------------------------

instance NFData Player where
  rnf x = seq x ()

instance NFData Board where
  rnf b = seq b ()

instance NFData Score where
  rnf s = seq s ()

instance Ord Board where
  compare b1 b2 = case (turn b1, turn b2) of
    (H, V) -> LT
    (V, H) -> GT
    (_, _) -> case (compare (free b1) (free b2)) of
      EQ -> compare (hist b1) (hist b2)
      x  -> x

instance Arbitrary Player where
  arbitrary = elements [H,V]
------------------------------------------------------------------------------------------------------

-- for debugging only
instance Arbitrary Board where
  arbitrary = do
    b <- elements [ Sample.board 2 2
                  , Sample.board 2 3
                  , Sample.board 3 2
                  , Sample.board 3 3
                  , Sample.board 3 4
                  , Sample.board 4 3
                  , Sample.board 4 4
                  , Sample.hatch 2
                  , Sample.hatch 3
                  , Sample.hatch 4
                  , Sample.carpets !! 1
                  , Sample.carpets !! 2
                  , Sample.carpets !! 3
                  ]
    makeMove b

makeMove :: Board -> Gen Board
makeMove b = do
  coin <- elements [True, False]
  if not coin then return b
    else
      let moves = Sample.legalMoves (turn b) b in
      if null moves then return b
        else do
          c <- elements moves
          makeMove (Sample.moveLegal b c)

arbitrarySmallBoards :: Gen Board
arbitrarySmallBoards = elements [Sample.board 2 2, Sample.board 2 3, Sample.board 3 2, Sample.board 3 3, Sample.carpets !! 1]


instance Arbitrary (Tree Board) where
  arbitrary = do
    a <- elements [1..2]
    b <- arbitrarySmallBoards
    return $ Sample.prune a (Sample.gametree b)

instance CoArbitrary Board where
  coarbitrary b = variant 0

data Sfn = Sfn (Board -> Score) String

instance Show Sfn where
  show (Sfn sfn name) = name

instance Arbitrary Sfn where
  arbitrary = elements [ Sfn Sample.score "The scoring function from question 2."
                       , Sfn (\_ -> Win H) "The scoring function which always returns 'Win H'."
                       , Sfn (\_ -> Win V) "The scoring function which always returns 'Win V'."
                       , Sfn (\_ -> Heu 0) "The scoring function which always returns 'Heu 0'."
                       ]

------------------------------------------------------------------------------------------------------

-- Q1
-- legalMoves
prop_legalMoves_sub :: Player -> Board -> Property
prop_legalMoves_sub p b = Student.legalMoves p b `p_subset` Sample.legalMoves p b

prop_legalMoves_sub_partial :: Board -> Property
prop_legalMoves_sub_partial b = Student.legalMoves p b `p_subset` Sample.legalMoves p b
  where p = turn b

prop_legalMoves_sup :: Player -> Board -> Property
prop_legalMoves_sup p b = Student.legalMoves p b `p_superset` Sample.legalMoves p b

prop_legalMoves_sup_partial :: Board -> Property
prop_legalMoves_sup_partial b = Student.legalMoves p b `p_superset` Sample.legalMoves p b
  where p = turn b

-- moveLegal
-- too many discards, coz generated moves are mostly invalid.
prop_moveLegal_turn :: Board -> Cell -> Property
prop_moveLegal_turn b m = Sample.valid b m ==> (turn $ Student.moveLegal b m) ~= (Sample.opp $ turn b)

prop_moveLegal_free_sub :: Board -> Cell -> Property
prop_moveLegal_free_sub b m = Sample.valid b m ==> (free $ Student.moveLegal b m) `p_subset` (free $ Sample.moveLegal b m)

prop_moveLegal_free_sup :: Board -> Cell -> Property
prop_moveLegal_free_sup b m = Sample.valid b m ==> (free $ Student.moveLegal b m) `p_superset` (free $ Sample.moveLegal b m)

prop_moveLegal_hist :: Board -> Cell -> Property
prop_moveLegal_hist b m = Sample.valid b m ==> (hist $ Student.moveLegal b m) ~= (hist $ Sample.moveLegal b m)

prop_moveLegal_hist_partial :: Board -> Cell -> Property
prop_moveLegal_hist_partial b m = Sample.valid b m ==> (hist $ Student.moveLegal b m) `p_permOf` (hist $ Sample.moveLegal b m)

-- replay
-- needs to generate boards with history
prop_replay_empty :: Board -> Property
prop_replay_empty b = hist b == [] ==> Student.replay b `p_eqboardlist` [b]

prop_replay_last :: Board -> Property
prop_replay_last b = hist b /= [] ==> (last $ Student.replay b) `p_eqboard` b

prop_replay_last_partial :: Board -> Property
prop_replay_last_partial b = hist b /= [] ==> b `p_elem` Student.replay b

prop_replay_rest :: Board -> Property
prop_replay_rest b = hist b /= [] ==> (deleteBy eqboard b $ Student.replay b) `p_eqboardlist` (deleteBy eqboard b $ Sample.replay b)

prop_replay_rest_partial_star :: Board -> Property
prop_replay_rest_partial_star b = hist b /= [] ==> p_permOfBy eqboard (deleteBy eqboard b $ Student.replay b) (deleteBy eqboard b $ Sample.replay b)

prop_replay_rest_partial_free :: Board -> Property
prop_replay_rest_partial_free b = hist b /= [] ==> (map (sort . free) $ deleteBy eqboard b $ Student.replay b) ~= (map (sort.free) $ deleteBy eqboard b $ Sample.replay b)

prop_replay_rest_partial_hist :: Board -> Property
prop_replay_rest_partial_hist b = hist b /= [] ==> (map hist $ deleteBy eqboard b $ Student.replay b) ~= (map hist $ deleteBy eqboard b $ Sample.replay b)

prop_replay_rest_partial_turn :: Board -> Property
prop_replay_rest_partial_turn b = hist b /= [] ==> (map turn $ deleteBy eqboard b $ Student.replay b) ~= (map turn $ deleteBy eqboard b $ Sample.replay b)


-- Q2
-- score
scorecateq :: Score -> Score -> Bool
scorecateq (Win _) (Win _) = True
scorecateq (Heu _) (Heu _) = True
scorecateq _ _ = False

-- too many discards, needs to generate specific board
prop_score_win :: Board -> Property
prop_score_win b = Sample.score b == Win p ==> Student.score b ~= Win p
  where p = Sample.opp $ turn b

prop_score_heu :: Board -> Property
prop_score_heu b = Sample.score b `scorecateq` Heu 0 ==> Student.score b ~= Heu x
  where x = length (Student.legalMoves V b) - length (Student.legalMoves H b) - sign (turn b)
        sign :: Player -> Int
        sign H = -1
        sign V = 1

prop_score_heu_partial :: Board -> Property
prop_score_heu_partial b = Sample.score b `scorecateq` Heu 0 ==>
  let x = Student.score b in
  let y = Heu 0 in
  counterexample ("Your output " ++ show x ++ " is not of type 'Heu x' but the correct output is " ++ show y ++ ".")
  (deepseq x x `scorecateq` deepseq y y)

-- minimax

prop_minimax_empty :: Board -> Sfn -> Property
prop_minimax_empty b (Sfn sfn _) = Student.minimax sfn (Node b []) ~= Node (b, sfn b) []

prop_minimax_fmap :: Tree Board -> Sfn -> Property
prop_minimax_fmap t (Sfn sfn _) = fmap fst (Student.minimax sfn t) ~= t

prop_minimax_H :: Tree Board -> Sfn -> Property
prop_minimax_H (Node b ts) (Sfn sfn _) = turn b == H && ts /= [] ==> Student.minimax sfn (Node b ts) ~= Node (b, minimum ss) ts'
  where ts' = map (Student.minimax sfn) ts
        ss = [s | Node (_,s) _ <- ts']

prop_minimax_V :: Tree Board -> Sfn -> Property
prop_minimax_V (Node b ts) (Sfn sfn _) = turn b == V && ts /= [] ==> Student.minimax sfn (Node b ts) ~= Node (b, maximum ss) ts'
  where ts' = map (Student.minimax sfn) ts
        ss = [s | Node (_,s) _ <- ts']

-- bestmoves
prop_bestmoves_sub :: Board -> Sfn -> Property
prop_bestmoves_sub b (Sfn sfn _) = Student.bestmoves 1000 sfn b `p_subset` Sample.bestmoves 1000 sfn b

prop_bestmoves_sub_partial :: Board -> Sfn -> Property
prop_bestmoves_sub_partial b (Sfn sfn _) = Student.bestmoves 1000 sfn b `p_subset` Student.legalMoves (turn b) b

prop_bestmoves_sup :: Board -> Sfn -> Property
prop_bestmoves_sup b (Sfn sfn _) = Student.bestmoves 1000 sfn b `p_superset` Sample.bestmoves 1000 sfn b

-- it just returns True because we are testing it using timeout
-- you might want to tweak the size of the board before testing
prop_bestmoves_pruning :: Property
prop_bestmoves_pruning = seq (Student.bestmoves 2 (Sample.score) (Sample.carpets !! 2)) $ property True


-- Q3
instance PickingMonad Identity where
  pick lo hi = Identity lo

prop_runGame_id_H :: Board -> Property
prop_runGame_id_H b = turn b == H ==>
  let Identity newB = Student.runGame (\_ -> return Nothing) (\_ -> return Nothing) b in
  newB `p_eqboard` b

prop_runGame_id_V :: Board -> Property
prop_runGame_id_V b = turn b == V ==>
  let Identity newB = Student.runGame (\_ -> return Nothing) (\_ -> return Nothing) b in
  newB `p_eqboard` b

prop_runGame_illegal_move_H :: Board -> Cell -> Property
prop_runGame_illegal_move_H b m = turn b == H && not (Sample.valid b m) ==> monadicIO $ do
  newB <- run $ Student.runGame (\_ -> return $ Just m) (\_ -> return Nothing) b
  monitor (\_ -> newB `p_eqboard` b)

prop_runGame_illegal_move_V :: Board -> Cell -> Property
prop_runGame_illegal_move_V b m = turn b == V && not (Sample.valid b m) ==> monadicIO $ do
  newB <- run $ Student.runGame (\_ -> return Nothing) (\_ -> return $ Just m) b
  monitor (\_ -> newB `p_eqboard` b)

-- not sure if you wanted to generate functions
-- we should prolly just fix playH and playV otherwise it would generate a lot of illegal moves
prop_runGame_H :: Board -> Property
prop_runGame_H b =
  let playH = Sample.randomPlay in
  let playV = Sample.randomPlay in
  turn b == H && (case playH b of Identity Nothing -> False
                                  Identity (Just m) -> Sample.valid b m) ==>
  monadic runIdentity $ do
    Just m <- run $ playH b
    newB <- run $ Student.runGame playH playV b
    rB <- run $ Student.runGame playH playV (Sample.moveLegal b m)
    monitor (\_ -> newB `p_eqboard` rB)

prop_runGame_V :: Board -> Property
prop_runGame_V b =
  let playH = Sample.randomPlay in
  let playV = Sample.randomPlay in
  turn b == V && (case playV b of Identity Nothing -> False
                                  Identity (Just m) -> Sample.valid b m) ==>
  monadic runIdentity $ do
    Just m <- run $ playV b
    newB <- run $ Student.runGame playH playV b
    rB <- run $ Student.runGame playH playV (Sample.moveLegal b m)
    monitor (\_ -> newB `p_eqboard` rB)

prop_runGame_list :: Board -> Property
prop_runGame_list b =
  let playH = Sample.randomPlay in
  let playV = Sample.randomPlay in
  length (free b) <= 10 ==>
  Student.runGame playH playV b `p_seteq` Sample.runGame playH playV b


-- Q4
translate :: Board -> Board
translate b = Board {turn = turn b, free = map shift (free b), hist = map shift (hist b)}
  where (i,j) = minimum $ free b
        shift = \(x,y) -> (x-i,y-j)

prop_carpets_zero :: Property
prop_carpets_zero = (free $ translate $ Student.carpets !! 0) `p_permOf` (free $ translate $ Sample.carpets !! 0)

prop_carpets_hist :: Int -> Property
prop_carpets_hist n = n >= 0 ==>
  (hist $ Student.carpets !! n) ~= []

prop_carpets_turn_H :: Int -> Property
prop_carpets_turn_H n = n >= 0 ==>
  (turn $ Student.carpets !! (2 * n)) ~= H

prop_carpets_turn_V :: Int -> Property
prop_carpets_turn_V n = n >= 0 ==>
  (turn $ Student.carpets !! (2 * n + 1)) ~= V

prop_carpets_free :: Int -> Property
prop_carpets_free n = n > 0 && n <= 4 ==>
  (free $ translate $ Student.carpets !! n) `p_permOf` (free $ translate $ Sample.carpets !! n)

prop_carpets_free_partial_length :: Int -> Property
prop_carpets_free_partial_length n = n > 0 && n <= 4 ==>
  (length $ free $ Student.carpets !! n) ~= (length $ free $ Sample.carpets !! n)

prop_carpets_free_partial_nub :: Int -> Property
prop_carpets_free_partial_nub n = n > 0 && n <= 4 ==>
  (nub $ free $ translate $ Student.carpets !! n) `p_permOf` (free $ translate $ Sample.carpets !! n)
