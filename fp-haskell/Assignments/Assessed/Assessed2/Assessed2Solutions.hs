-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed2 (choose , simulate , cut , shuffle , riffles , permute , genTree) where

import Types
import Data.List

-- Extra import
import Control.Monad

standard52 :: Deck
standard52 = [Card {rank = r, suit = s} | r <- [R2 .. RA], s <- [C .. S]]

code :: PickingMonad m => m Char
code = do
  i <- pick 0 3
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalise :: Eq a => Dist a -> Dist a
normalise xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub" removes duplicates from a list

-- Question 1
--- Part 1
instance Show Rank where
  show R2  = "2"
  show R3  = "3"
  show R4  = "4"
  show R5  = "5"
  show R6  = "6"
  show R7  = "7"
  show R8  = "8"
  show R9  = "9"
  show R10 = "10"
  show RJ  = "J"
  show RQ  = "Q"
  show RK  = "K"
  show RA  = "A"

---- Using fromEnum
-- instance Show Rank where
--   show RA = "A"
--   show RK = "K"
--   show RQ = "Q"
--   show RJ = "J"
--   show x  = show . (+2) . fromEnum $ x

--- Part 2
---- Using unicode characters
instance Show Suit where
  show C = "♣"
  show D = "♦"
  show H = "♥"
  show S = "♠"

---- Using escape codes
-- instance Show Suit where
--   show C = "\9827"
--   show D = "\9830"
--   show H = "\9829"
--   show S = "\9824"

red, black :: String -> String
red   s = "\x1b[31m" ++ s ++ "\x1b[0m"
black s = "\x1b[30m" ++ s ++ "\x1b[0m"

--- Part 3
---- Using pattern matching
instance Show Card where
  show (Card rank suit) = let text = show rank ++ show suit in case suit of
    D -> red text
    H -> red text
    _ -> black text

---- Using 'rank' and 'suit'
-- instance Show Card where
--   show card = let (r,s) = (rank card,suit card) in
--               if s `elem` [D,H]
--               then red (show r ++ show s)
--               else black (show r ++ show s)

-- Question 2
--- Part 1
---- Using do notation
choose :: PickingMonad m => [a] -> m a
choose xs = do
  i <- pick 0 (length xs - 1)
  return (xs !! i)

---- Using '<$>', provide error message for empty list
choose_1 :: PickingMonad m => [a] -> m a
choose_1 [] = error "Can't choose element from an empty list"
choose_1 xs = (!!) xs <$> pick 0 (length xs - 1)

--- Part 2
---- Using do notation
simulate :: Monad m => m Bool -> Integer -> m Integer
simulate bm 0 = return 0
simulate bm n = do
  k <- simulate bm (n-1)
  b <- bm
  if b
  then return (k+1)
  else return k

---- Using '>>=' and '<$>', provide error message for n < 0
simulate_1 :: Monad m => m Bool -> Integer -> m Integer
simulate_1 bm n
  | n < 0 = error "Cannot simulate a negative number of time"
  | n == 0 = return 0
  | otherwise = bm >>= \trial -> (if trial then (+1) else id) <$> simulate_1 bm (n-1)

---- Using replicateM
simulate_2 :: Monad m => m Bool -> Integer -> m Integer
simulate_2 bm n = genericLength . filter id <$> replicateM (fromIntegral n) bm

-- Question 3
--- Part 1
cut :: PickingMonad m => [a] -> m ([a],[a])
cut xs = do
  i <- pick 0 (length xs)
  return (splitAt i xs)

--- Part 2
---- Using nested 'do'
shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle (xs,[])     = return xs
shuffle ([],ys)     = return ys
shuffle (x:xs,y:ys) = do
  let m = length (x:xs)
  let n = length (y:ys)
  k <- pick 1 (m + n)
  if k <= m
  then do
    zs <- shuffle (xs,y:ys)
    return (x:zs)
  else do
    zs <- shuffle (x:xs,ys)
    return (y:zs)

---- Using '<$>'
shuffle_1 :: PickingMonad m => ([a],[a]) -> m [a]
shuffle_1 ([], ys) = return ys
shuffle_1 (xs, []) = return xs
shuffle_1 (x:xs, y:ys) = do
  let m = length (x:xs)
  let n = length (y:ys)
  k <- pick 1 (m + n)
  if k <= m
    then (x:) <$> shuffle_1 (xs,y:ys)
    else (y:) <$> shuffle_1 (x:xs,ys)

--- Part 3
riffles :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles cf sf n xs
  | n < 0 = error "Can't perform a negative number of shuffles"
  | n == 0 = return xs
  | otherwise = do
      cutDeck <- cf xs
      shuffledDeck <- sf cutDeck
      riffles cf sf (n-1) shuffledDeck

---- Doing the recursive part first
riffles_1 :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles_1 cf sf 0 xs = return xs
riffles_1 cf sf n xs = do
  xs' <- riffles cf sf (n-1) xs
  (ys,zs) <- cf xs'
  sf (ys,zs)

---- Using higher order functions
riffles_2 :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles_2 cf sf n xs = foldM (\ys _ -> cf ys >>= sf) xs [1..n]

-- Question 4
--- Part 1
permute :: PickingMonad m => [a] -> m [a]
permute [] = return []
permute (x:xs) = do
  ys <- permute xs
  insertionPoint <- pick 0 (length ys)
  return (insertAt insertionPoint x ys)
    where
      insertAt 0 x ys     = x : ys
      insertAt n x []     = error "Inserted past end of list"
      insertAt n x (y:ys) = y: insertAt (n-1) x ys
---- Can also have (less efficiently)
---- 'insertAt n x ys = take n ys ++ [x] ++ drop n ys'

permute_2 :: PickingMonad m => [a] -> m [a]
permute_2 [] = return []
permute_2 (x:xs) = do
  result <- permute_2 xs
  (ys,zs) <- cut result
  return (ys ++ [x] ++ zs)

---- Here is a *NON*-solution using the library function "permutations":
permute_bad :: PickingMonad m => [a] -> m [a]
permute_bad xs = choose (permutations xs)
-- Why is this solution not acceptable?  Well, think about what
-- actually happens if we try executing it in the IO monad (hint: the
-- running time will *not* be quadratic in the length!).  Indeed, if
-- we turn on profiling in ghci, it's particularly revealing to pay
-- attention to memory usage:
{-
*Assessed2> :set +s
*Assessed2> permute_bad [1..6]
[1,6,3,5,2,4]
(0.01 secs, 342,584 bytes)
*Assessed2> permute_bad [1..7]
[1,2,6,3,5,7,4]
(0.01 secs, 1,763,384 bytes)
*Assessed2> permute_bad [1..8]
[4,1,8,2,3,7,6,5]
(0.02 secs, 12,885,040 bytes)
*Assessed2> permute_bad [1..9]
[5,7,9,8,1,3,2,4,6]
(0.23 secs, 111,736,504 bytes)
*Assessed2> permute_bad [1..10]
[10,3,2,8,5,1,4,9,6,7]
(1.39 secs, 1,092,293,904 bytes)
-}
-- That's right, over 1GB of memory to generate a permutation of length ten!!
-- Compare this with our solution for permute:
{-
*Assessed2> permute [1..10]
[7,1,3,5,6,2,9,8,10,4]
(0.01 secs, 115,064 bytes)
*Assessed2> permute [1..100]
[17,31,35,24,62,91,2,73,66,20,75,32,33,23,97,26,71,22,41,74,28,58,82,78,70,30,77,29,1,69,96,83,53,5,72,88,68,4,52,18,48,14,99,56,79,84,86,37,98,13,34,63,27,11,12,42,80,16,8,59,61,55,39,7,81,65,90,19,50,6,64,47,25,36,76,57,93,87,46,92,3,51,85,67,44,95,54,94,9,21,38,45,89,40,100,49,15,60,10,43]
(0.02 secs, 1,450,744 bytes)
*Assessed2> permute [1..1000] >>= \xs -> return (length xs)  -- now we just print out the length, for brevity
1000
(0.29 secs, 98,138,472 bytes)
-}
-- A huge improvement...although ~100MB of memory to generate a permutation of
-- of length 1000 still seems like a lot...

---- Knuth's "Algorithm P" is a more efficient way of generating
---- random permutations, taking linear time and space.  It is based
---- on repeated swaps in an array, rather than on repeated insertions
---- into a list.  The following code implements Algorithm P,
---- representing the array as a function from indices to values.
permuteP :: PickingMonad m => [a] -> m [a]
permuteP xs = do
  f <- permutearr (length xs-1) (\i -> xs !! i)
  return [f i | i <- [0..length xs-1]]
  where
    swap :: Int -> Int -> (Int -> a) -> (Int -> a)
    swap i j f k
      | k == j    = f i
      | k == i    = f j
      | otherwise = f k
    permutearr :: PickingMonad m => Int -> (Int -> a) -> m (Int -> a)
    permutearr 0 f = return f
    permutearr i f = pick 0 i >>= \j -> permutearr (i-1) (swap i j f)
-- Some performance metrics:
{-
*Assessed2> permuteP [1..1000] >>= \xs -> return (length xs)
1000
(0.02 secs, 1,891,040 bytes)
*Assessed2> permuteP [1..10000] >>= \xs -> return (length xs)
10000
(0.05 secs, 18,308,200 bytes)
*Assessed2> permuteP [1..100000] >>= \xs -> return (length xs)
100000
(0.21 secs, 182,466,744 bytes)
-}
-- Can you do better?...

--- Part 2
---- Helper function to get number of subtrees of a tree
size :: Bin a -> Int
size (L _)   = 1
size (B l r) = 1 + size l + size r

genTree :: PickingMonad m => [a] -> m (Bin a)
genTree [] = error "Empty list"
genTree [x] = return (L x)
genTree (x:xs) = do
  -- Apply genTree to tail
  t <- genTree xs
  -- Pick a number between 1 and number of subtrees
  n <- pick 1 (size t)
  {- Alternatively, can use the identity nodes = 2*leaves-1 to avoid computing size:
  n <- pick 1 (2*length xs-1) -}
  -- Flip a coin to insert on left or right
  goLeft <- choose [True,False]
  -- Insert the node into the nth subtree
  return $ insertIntoTree x n goLeft t
    where
      insertIntoTree :: a -> Int -> Bool -> Bin a -> Bin a
      insertIntoTree y n left (L z) = if left
                                      then B (L y) (L z)
                                      else B (L z) (L y)
      insertIntoTree y n left (B l r)
        | n <= s = B (insertIntoTree y n left l) r
        | n == s + 1 = if left
                       then B (L y) (B l r)
                       else B (B l r) (L y)
        | otherwise = B l (insertIntoTree y (n - s - 1) left r)
        where s = size l

---- A more efficient version, using success and failure continuations to implement insertIntoTree
genTree_1 :: PickingMonad m => [a] -> m (Bin a)
genTree_1 [] = error "Empty list"
genTree_1 [x] = return (L x)
genTree_1 (x:xs) = do
  t <- genTree_1 xs
  n <- pick 1 (size t)
  goLeft <- choose [True,False]
  return $ insertIntoTree x n goLeft t id (\_ -> error "impossible index out of bounds")
    where
      insertIntoTree :: a -> Int -> Bool -> Bin a -> (Bin a -> r) -> (Int -> r) -> r
      insertIntoTree y 1 left t       succ fail = succ (if left then B (L y) t else B t (L y))
      insertIntoTree y n left (L _)   succ fail = fail (n-1)
      insertIntoTree y n left (B l r) succ fail = insertIntoTree y (n-1) left l (\l' -> succ (B l' r)) $ \n' ->
                                                  insertIntoTree y n' left r (\r' -> succ (B l r')) fail

---- A less efficient but conceptually simple version, using addresses to point to subtrees (as in the notes data.md)
type Direction = Either () ()
type Address   = [Direction]

validAddresses :: Bin a -> [Address]
validAddresses (L _)   = [[]]
validAddresses (B l r) = [[]]
                      ++ [Left  ():ds | ds <- validAddresses l]
                      ++ [Right ():ds | ds <- validAddresses r]

genTree_2 :: PickingMonad m => [a] -> m (Bin a)
genTree_2 [] = error "Empty list"
genTree_2 [x] = return (L x)
genTree_2 (x:xs) = do
  t <- genTree_2 xs
  ptr <- choose (validAddresses t)
  goLeft <- choose [True,False]
  return $ insertIntoTree x ptr goLeft t
    where
      insertIntoTree :: a -> Address -> Bool -> Bin a -> Bin a
      insertIntoTree y [] left t = if left then B (L y) t else B t (L y)
      insertIntoTree y (Left  ():ptr) left (B l r) = B (insertIntoTree y ptr left l) r
      insertIntoTree y (Right ():ptr) left (B l r) = B l (insertIntoTree y ptr left r)
      insertIntoTree y _              left (L _)   = error "invalid address"
