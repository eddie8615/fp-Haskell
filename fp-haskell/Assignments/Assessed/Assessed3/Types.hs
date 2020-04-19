module Types where

import System.Random

data Player = H | V
  deriving (Eq, Ord, Show)

type Cell = (Int, Int)

data Board = Board { turn :: Player, free :: [Cell], hist :: [Cell] }
  deriving (Eq, Show)

data Score = Win Player | Heu Int
  deriving (Show,Eq)

instance Ord Score where
  Win H <= _     = True
  _     <= Win V = True
  Heu x <= Heu y = x <= y
  _     <= _     = False

  min (Win H) _       = Win H
  min (Heu x) (Win V) = Heu x
  min (Heu x) (Heu y) = Heu (min x y)
  min (Heu x) (Win H) = Win H
  min (Win V) p       = p
  
  max (Win V) _       = Win V
  max (Heu x) (Win H) = Heu x
  max (Heu x) (Heu y) = Heu (max x y)
  max (Heu x) (Win V) = Win V
  max (Win H) p       = p


class Monad m => PickingMonad m where
  pick :: Int -> Int -> m Int

instance PickingMonad IO where
  pick lo hi | lo <= hi  = getStdRandom (randomR (lo, hi))
             | otherwise = error ("pick lo hi: lo = " ++ show lo ++ " is greater than hi = " ++ show hi)

instance PickingMonad [] where
  pick lo hi | lo <= hi  = [lo..hi]
             | otherwise = error ("pick lo hi: lo = " ++ show lo ++ " is greater than hi = " ++ show hi)

