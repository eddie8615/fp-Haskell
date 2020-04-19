-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Formative3 (emptyTrie , insertTrie , trieToList , findTrie , isEmpty , epsilon , wordrects) where

import Data.Char
import Data.List

readDict :: String -> IO [String]
readDict path = do
  content <- readFile path
  return (map (map toUpper) $ lines content)

data Trie = Trie [(Maybe Char, [Trie])] deriving (Show, Eq, Ord) -- replace this by an appropriate type, data, or newtype definition of your choice

-- construct the empty trie
emptyTrie :: Trie
emptyTrie = Trie [(Nothing, [])]

-- insert a string into a trie
insertTrie :: String -> Trie -> Trie
insertTrie [] t = t
insertTrie (s:str) t = let child = Trie [(Just s, [])]
                        in
                        case lookup s t of
                            Nothing ->
                            Just n -> insertTrie str n

-- given a trie, return the list of strings it contains
trieToList :: Trie -> [String]
trieToList = undefined

-- given a string xs and a trie t, return a (possibly empty) trie containing all the strings in t that have xs as a prefix
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
