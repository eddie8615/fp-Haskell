-- This solution uses prelude functions (loaded by default):
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] ys     = null ys
isPermutation (x:xs) ys = (x `elem` ys) && isPermutation xs (ys \\\ x)

-- (xs \\\ x) is xs with the first occurrence of x removed, if it
-- exists. It is undefined if it doesn't.

(\\\) :: Eq a => [a] -> a -> [a]
[]     \\\ _   = undefined
(x:xs) \\\ y
  | x == y     = xs
  | otherwise  = x : (xs \\\ y)

-- For testing. The following should be the constantly true function:
true1 :: Int -> Bool
true1 n = isPermutation [1..n] (reverse [1..n])
    
{-

* Backquotes are used to turn a function of two arguments into a binary
  infix operator, so that

    (x `elem` xs) = elem x xs

* You can use unused symbols as binary operator you define. When you
  want to mention them as functions, you write then in round
  braces. For example,

    (+) x y = x+y

* When there is no possible value, we can use undefined (a better
  approach is to use the Maybe type).

* We can use guards indicated with "|" instead of if-then-else.

* The function isPermutation is quadratic time in the length of the
  first list in the worst case. You can test this experimentally, by
  setting ":set +s" so that ghci prints run times, and trying (true1
  n) with increasing n.


     (2n)^2 = 4(n^2)

  Hence if we double n we should quadruple the run time:

  $ ghci permutation.hs
  GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
  *Main> true1 1500
  True
  (0.40 secs, 306,756,248 bytes)
  *Main> true1 (2 * 1500)
  True
  (1.58 secs, 1,225,444,912 bytes)
  *Main> true1 (2 * 2 * 1500)
  True
  (6.23 secs, 4,898,810,096 bytes)
  *Main> true1 (2 * 2 * 2 * 1500)
  True
  (24.76 secs, 19,589,546,368 bytes)
  *Main> true1 (2 * 2 * 2 * 2 * 1500)
  True
  (98.51 secs, 78,347,215,944 bytes)
  *Main> 

  Is there a more efficient algorithm? (If you only have Eq. If you
  have Ord, you can sort the two lists and compare for equality, which
  should give you an n*log(n) algorithm.)

-}
