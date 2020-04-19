module Types where

import Data.List
import System.Random

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
  deriving (Eq,Ord,Enum)
data Suit  = C | D | H | S
  deriving (Eq,Ord,Enum)
data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq)
type Deck = [Card]

class Monad m => PickingMonad m where
  pick :: Int -> Int -> m Int

instance PickingMonad IO where
  pick lo hi | lo <= hi  = getStdRandom (randomR (lo, hi))
             | otherwise = error ("pick lo hi: lo = " ++ show lo ++ " is greater than hi = " ++ show hi)

instance PickingMonad [] where
  pick lo hi | lo <= hi  = [lo..hi]
             | otherwise = error ("pick lo hi: lo = " ++ show lo ++ " is greater than hi = " ++ show hi)

newtype Dist a = Dist { dist :: [(a,Rational)] }  deriving (Show)

instance Monad Dist where
  return x = Dist [(x,1)]
  xm >>= f = Dist [(y,p*q) | (x,p) <- dist xm, (y,q) <- dist (f x)]

instance Functor Dist where
  fmap f xm = xm >>= return . f

instance Applicative Dist where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

instance PickingMonad Dist where
  pick lo hi | lo <= hi = Dist [(x,1 / fromIntegral (hi - lo + 1)) | x <- [lo..hi]]
             | otherwise = error ("pick lo hi: lo = " ++ show lo ++ " is greater than hi = " ++ show hi)

data Bin a = L a | B (Bin a) (Bin a)  deriving (Show,Eq)
