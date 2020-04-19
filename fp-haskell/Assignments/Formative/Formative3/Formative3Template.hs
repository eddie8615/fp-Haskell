-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Formative3 (emptyTrie , insertTrie , trieToList , findTrie , isEmpty , epsilon , wordrects) where

import Data.Char

readDict :: String -> IO [String]
readDict path = do
  content <- readFile path
  return (map (map toUpper) $ lines content)

type Trie = ()   -- replace this by an appropriate type, data, or newtype definition of your choice

-- construct the empty trie
emptyTrie :: Trie
emptyTrie = undefined

-- insert a string into a trie
insertTrie :: String -> Trie -> Trie
insertTrie = undefined

-- given a trie, return the list of strings it contains
trieToList :: Trie -> [String]
trieToList = undefined

-- given a string xs and a trie t, return a (possibly empty) trie containing all of the possible suffixes of xs in t
findTrie :: String -> Trie -> Trie
findTrie = undefined

-- return True if the trie is empty, or else False
isEmpty :: Trie -> Bool
isEmpty = undefined

-- return True if the trie contains the empty string "", or else False
epsilon :: Trie -> Bool
epsilon = undefined

wordrects :: [String] -> Int -> Int -> [String] -> [[String]]
wordrects dict rows cols crib = undefined

