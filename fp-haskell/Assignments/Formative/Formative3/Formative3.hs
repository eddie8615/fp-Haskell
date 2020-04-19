-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Formative3 (emptyTrie , insertTrie , trieToList , findTrie , isEmpty , epsilon , wordrects) where

import Data.Char
import Data.List
import qualified Data.Map as M

readDict :: String -> IO [String]
readDict path = do
  content <- readFile path
  return (map (map toUpper) $ lines content)

data Trie = Trie {value :: Maybe Char, children :: M.Map Char Trie}  deriving (Show,Eq,Ord) -- replace this by an appropriate type, data, or newtype definition of your choice

instance Show Trie

-- construct the empty trie
emptyTrie :: Trie
emptyTrie = Trie {value = Nothing, children = M.empty}

-- insert a string into a trie
insertTrie :: String -> Trie -> Trie
insertTrie [] t = t
insertTrie (s:str) t = let ts = children t
                           childNode = Trie {value = Just s, children = M.empty}
                           newChildren = M.insert s childNode ts
                       in case M.lookup s ts of
                            Nothing -> t {children = M.insert s (insertTrie str childNode) newChildren}
                            Just t' -> t {children = M.insert s (insertTrie str t') ts}

-- given a trie, return the list of strings it contains
trieToList :: Trie -> [String]
trieToList emptyTrie = []
trieToList t = undefined


-- given a string xs and a trie t, return a (possibly empty) trie containing all the strings in t that have xs as a prefix
findTrie :: String -> Trie -> Trie
findTrie [] t = t
findTrie (s:str) t = if M.member s (children t)
                     then findTrie str child
                     else emptyTrie
                     where child = (children t) M.! s

-- return True if the trie is empty, or else False
isEmpty :: Trie -> Bool
isEmpty t = if (children t) == M.empty
            then True
            else False

-- return True if the trie contains the empty string "", or else False
epsilon :: Trie -> Bool
epsilon = undefined



wordrects :: [String] -> Int -> Int -> [String] -> [[String]]
wordrects dict rows cols crib = undefined
