-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Main where

import Data.Char
import Formative3

readDict :: String -> IO [String]
readDict path = do
  content <- readFile path
  return (map (map toUpper) $ lines content)

main :: IO ()
main = do
  dict1 <- readDict "common10000.txt"
  dict2 <- readDict "english2.txt"  
  let l1 = length $ wordrects dict1 3 3 []
  let l2 = length $ wordrects dict2 3 3 []
  if (l1 + l2 == 619223)
  then putStrLn "You can enter the competition if the script was faster than 30 seconds.\n"
  else putStrLn "Unfortunately, your script is incorrect and so your script is not eligible for competition.\n"
