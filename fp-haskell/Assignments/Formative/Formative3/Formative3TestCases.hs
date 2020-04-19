module Formative3TestCases where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.DeepSeq
import Control.Monad.Identity

import qualified Formative3Solutions as Sample
import qualified Formative3 as Student

-- better version of ===
infix 4 ~=
(~=) :: (Eq a, Show a, NFData a) => a -> a -> Property
x ~= y =
  counterexample ("Your output was " ++ show x ++ ", but the correct output is " ++ show y ++ ".")
  (deepseq x x == deepseq y y)

p_elem' :: (Eq a, Show a, NFData a) => a -> [a] -> Property
p_elem' x xs = counterexample ("The input " ++ show x ++ " is not a member of your output " ++ show xs ++ ".")
             (deepseq x x `elem` deepseq xs xs)

p_elem :: (Eq a, Show a, NFData a) => a -> [a] -> Property
p_elem x xs = counterexample ("Your output " ++ show x ++ " is not a member of " ++ show xs ++ ".")
             (deepseq x x `elem` deepseq xs xs)

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

p_subset :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
p_subset xs ys = counterexample ("Your output " ++ show xs ++ " is not a subset of " ++ show ys ++ ".")
                 (xs `deepseq` ys `deepseq` (xs `subset` ys))

newtype PrintString = PrintString { getPrintString :: String } deriving (Eq, Ord, Read)

instance Show PrintString where
  show s = '"' : getPrintString s ++ "\""

instance Arbitrary PrintString where
  arbitrary = PrintString <$> listOf (choose ('a','z'))
  shrink (PrintString s) = map PrintString (liftShrink shrinkNothing s)



-------------------------------------------------------------

prop_empty_trie_does_not_contain_empty_string :: Property
prop_empty_trie_does_not_contain_empty_string = Student.epsilon Student.emptyTrie ~= False

prop_empty_trie_is_empty :: Property
prop_empty_trie_is_empty = Student.isEmpty Student.emptyTrie ~= True

prop_empty_trie_contains_nothing :: Property
prop_empty_trie_contains_nothing = Student.trieToList Student.emptyTrie ~= []

prop_trie_contains_one_after_insert :: PrintString -> Property
prop_trie_contains_one_after_insert (PrintString s) = length (Student.trieToList (Student.insertTrie s Student.emptyTrie)) ~= 1

prop_contains_empty_after_insert :: Property
prop_contains_empty_after_insert = Student.epsilon (Student.insertTrie "" Student.emptyTrie) ~= True

prop_non_empty_after_insert :: PrintString -> Property
prop_non_empty_after_insert (PrintString s) = Student.isEmpty (Student.insertTrie s Student.emptyTrie) ~= False

prop_trie_contains_string_after_insert :: PrintString -> Property
prop_trie_contains_string_after_insert (PrintString s) = s `p_elem'` Student.trieToList (Student.insertTrie s Student.emptyTrie)

prop_list_to_trie_to_list :: [PrintString] -> Property
prop_list_to_trie_to_list ss' =
  let
    ss = map getPrintString ss'
  in sort (Student.trieToList (foldr Student.insertTrie Student.emptyTrie ss)) ~= sort (nub ss)

prop_find_trie :: [PrintString] -> PrintString -> Property
prop_find_trie ss' (PrintString s) =
  let
    ss = map getPrintString ss'
  in counterexample ("'findTrie t " ++ show s ++ "' failed where 't' is the emptyTrie with " ++ show ss ++ " inserted") $ sort (Student.trieToList (Student.findTrie s (foldr Student.insertTrie Student.emptyTrie ss))) ~= sort (Sample.trieToList (Sample.findTrie s (foldr Sample.insertTrie Sample.emptyTrie ss)))

prop_word_rect_full :: IO [String] -> Int -> Int -> Property
prop_word_rect_full dict n m = once $ monadicIO $ do
  x <- run dict
  monitor (\_ -> Student.wordrects x n m [] ~= Sample.wordrects x n m [])

isRect :: Int -> Int -> [String] -> Property
isRect n m ans = length ans ~= n .&&. conjoin (map (\x -> length x ~= m) ans)

prop_word_rect_is_rect :: IO [String] -> Int -> Int -> Property
prop_word_rect_is_rect dict n m = once $ monadicIO $ do
  x <- run dict
  monitor (\_ -> conjoin (map (isRect n m) (Student.wordrects x n m [])))

correct :: [String] -> Int -> Int -> [String] -> Property
correct dict n m ans =
  let
    transpose :: [String] -> [String]
    transpose xs = if any null xs then [] else map head xs : transpose (map tail xs)
  in isRect n m ans .&&. ans `p_subset` dict .&&. transpose ans `p_subset` dict

prop_word_rect_one :: IO [String] -> Int -> Int -> Property
prop_word_rect_one dict n m = once $ monadicIO $ do
  x <- run dict
  let ans = Student.wordrects x n m []
  monitor (\_ -> if null ans then counterexample ("'wordRects' did not return any rectangles") False else correct x n m (head ans))

prop_all_word_rects_correct :: IO [String] -> Int -> Int -> Property
prop_all_word_rects_correct dict n m = once $ monadicIO $ do
  x <- run dict
  monitor (\_ -> conjoin (map (correct x n m) (Student.wordrects x n m [])))
