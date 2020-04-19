-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Formative3Solutions (emptyTrie , insertTrie , trieToList , findTrie , isEmpty ,
                   epsilon , wordrects) where

import Data.Char
import Data.List
import Data.Ord

readDict :: String -> IO [String]
readDict path = do
  content <- readFile path
  return (map (map toUpper) $ lines content)

-- Question 1
--- The boolean b in Node b cts signifies whether we have reached a word and not
--- just a prefix.
data Trie = Node Bool [(Char, Trie)] deriving Eq

--- construct the empty trie
emptyTrie :: Trie
emptyTrie = Node False []

--- insert a string into a trie
insertTrie :: String -> Trie -> Trie
insertTrie []     (Node _      cts) = Node True cts
insertTrie (c:cs) (Node isWord cts) = case find ((==c) . fst) cts of
  -- The node does not have the character c yet, so we add it.
  Nothing -> Node isWord ((c,insertTrie cs emptyTrie) : cts)
  -- The node does have the character c already, so we add the rest, cs, at the
  -- appropriate place and replace the old trie by the new, updated one.
  Just ct -> Node isWord ((c,insertTrie cs (snd ct))  : delete ct cts)

--- Alternatively, if we wish for something that works without having Eq on Trie
insertTrie_1 :: String -> Trie -> Trie
insertTrie_1 []     (Node _      cts) = Node True cts
insertTrie_1 (c:cs) (Node isWord cts) = Node isWord ((c,t) : cts')
  where
    -- Use Hoogle to find functions like break
    (cts1,cts2) = break ((==c) . fst) cts
    -- cts1 contains all character,trie-tuples up to (and excluding) the one
    -- whose character is c.
    -- cts2 contains the rest of the tuples.
    (t,cts') = -- If the character c does not (yet) appear at the node,
               if null cts2
               -- then add a new child,
               then (insertTrie cs emptyTrie, cts1)
                -- else, insert the rest at the right spot, and return the
                -- children, but skipping over the one with character c.
               else (insertTrie cs (snd $ head cts2), cts1 ++ tail cts2)

--- given a trie, return the list of strings it contains
trieToList :: Trie -> [String]
trieToList (Node isWord cts) = if isWord
                                then -- pause to save the current word
                                  [] : prepend cts
                                else -- keep adding characters
                                  prepend cts
  where
    -- prepend the character to the strings
    prepend :: [(Char,Trie)] -> [String]
    prepend = concatMap $ \(c,t) -> map (c:) (trieToList t)

--- given a string xs and a trie t, return a (possibly empty) trie containing all
--- of the possible suffixes of xs in t
findTrie :: String -> Trie -> Trie
findTrie []     t            = t
findTrie (c:cs) (Node _ cts) = case find ((==c) . fst) cts of
  Nothing    -> emptyTrie
  Just (_,t) -> findTrie cs t

--- return True if the trie is empty, or else False
isEmpty :: Trie -> Bool
isEmpty (Node isWord cts) = not isWord && null cts

--- return True if the trie contains the empty string "", or else False
epsilon :: Trie -> Bool
epsilon (Node isWord _) = isWord

-- Question 2

--- A naive and slow solution, not using tries.
wordrectsWithoutCrib :: [String] -> Int -> Int -> [[String]]
wordrectsWithoutCrib words m n = solutions
  where
   colWords  = filter ((==m).length) words
   rowWords  = filter ((==n).length) words
   potential = sequence (replicate n colWords)
   solutions = [ p | p <- potential,
                     all (\xs -> xs `elem` rowWords) (transpose p) ]

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] ys = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

wordrectsNaive :: [String] -> Int -> Int -> [String] ->  [[String]]
wordrectsNaive words m n crib =
  filter (isPrefix crib) (wordrectsWithoutCrib words m n)

--- Some helper functions for tries

--- decide whether a string occurs as a prefix of a string in a trie
isPrefixTrie :: String -> Trie -> Bool
isPrefixTrie s t = not . isEmpty $ findTrie s t

--- insert a list of strings in a trie
listInsertTrie :: [String] -> Trie
listInsertTrie =  foldr insertTrie emptyTrie

--- decide whether a string is contained in a trie
inTrieP :: String -> Trie -> Bool
inTrieP s t = epsilon $ findTrie s t

{- Our first continuations-based backtracking solver, using tries. -}

{- We write a continuations-based backtracking solver for word rectangles, using
   success and failure continuations.

   The parameters of "solve rowwords colwords crib n succ fail" are
   interpreted as follows:

   rowwords = list of valid words for rows
   colwords = trie of valid words for columns
   crib     = initial list of rows that we want to extend to a word rectangle
   n        = number of additional rows in the desired word rectangle
   succ     = success continuation, taking as argument a valid word rectangle
              + a resumption for collecting additional solutions
   fail     = failure continuation -}
solve :: [String] -> Trie -> [String] -> Int -> ([String] -> r -> r) -> r -> r
solve dict trie crib 0 succ fail
  -- transpose (Hoogle!) flips the rows and columns.
  | all (`inTrieP` trie) (transpose crib) = succ crib fail
  | otherwise                             = fail
solve dict trie crib n succ fail = foldr next fail newRows
  where
    next w fail' = solve dict trie (crib ++ [w]) (n-1) succ fail'
    -- add a word and check that the resulting columns are prefixes of words
    newRows = [w | w <- dict,
                   all (`isPrefixTrie` trie) (transpose (crib ++ [w]))]

--- wordrects calls the solver with appropriate success and failure continuations
wordrects :: [String] -> Int -> Int -> [String] -> [[String]]
wordrects dict rows cols crib =
  solve rowwords colwords crib (rows - length crib) (:) []
    where
      -- restrict the words for rows to have length #cols
      rowwords = filter ((==cols) . length) dict
      -- restrict the words for columns to have length #rows
      colwords = listInsertTrie $ filter ((==rows) . length) dict

{- Our second continuations-based backtracking solver. It is not faster than the
   first, but it serves as preparation for the third and fourth solvers (which
   are faster). -}

--- given a string xs and a trie t, return the list of characters that can
--- possibly follow xs in a string contained in t
validNext :: String -> Trie -> [Char]
validNext cs t = map fst cts
  where
    Node _ cts = findTrie cs t

--- We add the desired total number of columns as an argument before crib, and we
--- only change newRows.
solve_1 :: [String] -> Trie -> Int -> [String] -> Int
        -> ([String] -> r -> r) -> r -> r
solve_1 dict trie cols crib 0 succ fail
  | all (`inTrieP` trie) (transpose crib) = succ crib fail
  | otherwise                             = fail
solve_1 dict trie cols crib n succ fail = foldr next fail newRows
  where
    next w fail' = solve_1 dict trie cols (crib ++ [w]) (n-1) succ fail'
    newRows = if null crib
              then dict
              -- Only consider words that have the right characters for each
              -- column.
              else [w | w <- dict,
                        all (\k -> w !! k `elem` validNext (cribT !! k) trie)
                            [0..cols-1]]
      where
        cribT = transpose crib

wordrects_1 :: [String] -> Int -> Int -> [String] -> [[String]]
wordrects_1 dict rows cols crib =
  solve_1 rowwords colwords cols crib (rows - length crib) (:) []
    where
      rowwords = filter ((==cols) . length) dict
      colwords = listInsertTrie $ filter ((==rows) . length) dict

{- Our third continuations-based backtracking solver. It is quite a bit faster
   than the first two.

   The idea is to change newRows so that the number of possibilities is taken
   into account. For example, suppose that I told you that I'm looking for all
   words (of length 5, say), that have an 'A','B', 'C', 'D', 'E' or 'F' as the
   first letter and a 'G' or 'H' as the third letter. You would be wise to first
   look for all words satisfying the second requirement, before turning to the
   first requirement. This is because the number of words satisfying the second
   requirement is much smaller than the number of words satisfying the first
   requirement. Thus, by looking at the second requirement first, you quickly
   reduce your search space.
-}

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

thd3 :: (a,b,c) -> c
thd3 (_,_,z) = z

--- Again, we only change newRows. In this case, so that it considers the
--- requirements with fewest possibilities first.
solve_2 :: [String] -> Trie -> Int -> [String] -> Int
        -> ([String] -> r -> r) -> r -> r
solve_2 dict trie cols crib 0 succ fail
  | all (`inTrieP` trie) (transpose crib) = succ crib fail
  | otherwise                             = fail
solve_2 dict trie cols crib n succ fail = foldr next fail newRows
  where
    next w fail' = solve_2 dict trie cols (crib ++ [w]) (n-1) succ fail'
    newRows = if null crib
              then dict
              else [w | w <- dict,
                        all (\k -> (w !! (snd3 $ exts !! k) `elem`
                                   (thd3 $ exts !! k))) [0..cols-1]]
      where
        -- Sort by the number of possible characters.
        exts = sortBy (comparing fst3)
               [(length (exts' k), k, exts' k) | k <- [0..cols-1]]
          where
            -- As before.
            exts' :: Int -> [Char]
            exts' k = validNext ((transpose crib) !! k) trie

-- As before.
wordrects_2 :: [String] -> Int -> Int -> [String] -> [[String]]
wordrects_2 dict rows cols crib =
  solve_2 rowwords colwords cols crib (rows - length crib) (:) []
    where
      rowwords = filter ((==cols) . length) dict
      colwords = listInsertTrie $ filter ((==rows) . length) dict

{- Our fourth (and final) continuations-based backtracking solver. It is about as
   fast as the third.

   The idea is similar to the previous one. However, we reason that the number
   of words that have 'X' or 'Z' as the third letter is (usually) smaller than
   the number of words that have 'E' as the second letter, even though "'X' or
   'Z'" gives two possibilities rather than one ("'E' as the second
   letter"). This is because the letter the 'E' is much more common than the
   letters 'X' and 'Z'.

   Therefore, our fourth solver not only checks the number of possible
   characters, but it also takes their frequency into account.
-}

-- Character frequencies based on the English language
-- https://en.wikipedia.org/wiki/Letter_frequency
freq :: Char -> Int
freq c = case c of
  'A' -> 817
  'B' -> 149
  'C' -> 220
  'D' -> 425
  'E' -> 1270
  'F' -> 223
  'G' -> 202
  'H' -> 609
  'I' -> 697
  'J' -> 15
  'K' -> 129
  'L' -> 403
  'M' -> 241
  'N' -> 675
  'O' -> 751
  'P' -> 193
  'Q' -> 10
  'R' -> 599
  'S' -> 633
  'T' -> 936
  'U' -> 276
  'V' -> 98
  'W' -> 256
  'X' -> 15
  'Y' -> 199
  'Z' -> 7
  _   -> 1 -- anything else (e.g. ' in english2.txt) will be considered rare

-- We only change exts, compared to before.
solve_3 :: [String] -> Trie -> Int -> [String] -> Int
        -> ([String] -> r -> r) -> r -> r
solve_3 dict trie cols crib 0 succ fail
  | all (`inTrieP` trie) (transpose crib) = succ crib fail
  | otherwise                             = fail
solve_3 dict trie cols crib n succ fail = foldr next fail newRows
  where
    next w fail' = solve_3 dict trie cols (crib ++ [w]) (n-1) succ fail'
    newRows = if null crib
              then dict
              else [w | w <- dict,
                        all (\k -> (w !! (snd3 $ exts !! k) `elem`
                                   (thd3 $ exts !! k))) [0..cols-1]]
      where
        -- Take the character frequencies into account.
        exts = sortBy (comparing fst3)
               [(sum (map freq (exts' k)), k, exts' k) | k <- [0..cols-1]]
          where
            exts' :: Int -> [Char]
            exts' k = validNext ((transpose crib) !! k) trie

-- As before.
wordrects_3 :: [String] -> Int -> Int -> [String] -> [[String]]
wordrects_3 dict rows cols crib =
  solve_3 rowwords colwords cols crib (rows - length crib) (:) []
    where
      rowwords = filter ((==cols) . length) dict
      colwords = listInsertTrie $ filter ((==rows) . length) dict
