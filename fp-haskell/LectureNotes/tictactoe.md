# Unbeatable tic-tac-toe

```
               | O | O
            ---+---+---
             O | X | O
            ---+---+---
             X | X | X
```

  This is code from chapter 11 of our adopted book [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html), 2nd edition,
by [Graham Hutton](http://www.cs.nott.ac.uk/~pszgmh/), Cambridge University Press, 2016, with annotations and additions by the module lecturers. **Check the book for a full account.**

## Basic declarations

```haskell
import Data.Char
import Data.List
import System.IO
```

By default, the board is `3 × 3`, but we can change that:

```haskell
size :: Int
size = 3
```
We represent a grid as a list of rows, whether a row is a list of players

```haskell
type Grid = [[Player]]
```

and where a player is `O`, `B` (blank) or `X`:

```haskell
data Player = O | B | X
              deriving (Eq, Ord, Show)
```

We have `O < B < X`. This will be important when we consider the
`minimax` algorithm to play the game optimally. Player `O` wants to
minimize the outcome of the game, player `X` wants to maximize it. But
them game may be a draw, which we will represent by `B`. The
traditional convention in game theory is to use `-1,0,1` rather than
`O,B,X` for the outcome of the game, but we will follow the book,
using the type `Player` to encode players sometimes, and to encode the
actual outcome of a complete game, and to encode the optimal outcome
of an incomplete game.

```haskell
next :: Player -> Player
next O = X
next B = B
next X = O
```

## Grid utilities

The empty grid:

```haskell
empty :: Grid
empty = replicate size (replicate size B)
```
Check whether a grid is full:
```haskell
full :: Grid -> Bool
full = all (/= B) . concat
```
Check who plays next:
```haskell
turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)
            ps = concat g
```
Check whether a given player wins in a given grid:

```haskell
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]
```

Give a grid, extract its main diagonal as a list:

```haskell
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]
```

Check whether one of the (non-blank) players `O` or `X` wins:

```haskell
won :: Grid -> Bool
won g = wins O g || wins X g
```

# Displaying a grid

```haskell
putGrid :: Grid -> IO ()
putGrid =
   putStrLn . unlines . concat . interleave bar . map showRow
   where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
             beside = foldr1 (zipWith (++))
             bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys
```

## Making a move

For `size = 3`, the board and moves are

```
             0 | 1 | 2
            ---+---+---
             3 | 4 | 5
            ---+---+---
             6 | 7 | 8
```
More generally the moves are numbers between `0` and `size²-1` inclusive.
```haskell
type Move = Int
```
Check whether a move is `valid` and available:
```haskell
valid :: Grid -> Move -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B
```
Perform a move, with the convention that `[]` indicates failure, `[g]` indicates success with result `g`, and lists with more than one element are not used (hence we could have used the `Maybe` type instead):
```haskell
move:: Grid -> Move -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
             where (xs,B:ys) = splitAt i (concat g)
```
Break a list into maximal segments of a given length:
```haskell
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
```

## Reading a natural number

```haskell
getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt
```

## Human vs. human

We play in a terminal.
```haskell
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                    []   -> do putStrLn "ERROR: Invalid move"
                               run' g p
                    [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "
```
Clear screen:
```haskell
cls :: IO ()
cls = putStr "\ESC[2J"
```
Goto a position in the screen:
```haskell
goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
```

## Game trees

```haskell
data Tree a = Node a [Tree a]
              deriving Show
```
Create the game tree for a given grid and starting player:
```haskell
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]
```
The list of all available moves in a grid for a given player:
```haskell
moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]
```

If a game tree is too big, we may need to prune it in practice:
```haskell
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]
```
The default pruning depth:
```haskell
depth :: Int
depth = 9
```

## Minimax

Standard [minimax algorithm](https://en.wikipedia.org/wiki/Minimax).

Here we have a different interpretation of the type `Player`:

 * `O` means that `O` wins,
 * `X` means that `X` wins, and
 * `B` means that the game is a draw.

We have `O < B < X`.

```haskell
minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts)
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']
```

Use the game tree to compute a best move for a player in a given grid, and then play it:
```haskell
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree
```

## Human vs computer

```haskell
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 play' g p
                      [g'] -> play g' (next p)
   | p == X   = do putStr "Player X is thinking... "
                   (play $! (bestmove g p)) (next p)
```

## Lazy alpha-beta pruning (addition by your lecturers)

Given that tic-tac-toe is such a simple game, the run time of the
above algorithm is disappointing.  It takes a long time to determine an
optimal move to play first:
```
*Main> bestmove empty X
[[X,B,B],[B,B,B],[B,B,B]]
(18.54 secs, 12,373,859,944 bytes)
```

We will provide an improved version which is `9 times faster` - an
[order of magnitude](https://en.wikipedia.org/wiki/Order_of_magnitude),
in a cheap way.

We exploit laziness to perform a cheap version of [α-β-pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning).

We'll soon have a lecture about (the meaning and exploitation of) laziness.

Replace the `minimum` and `maximum` functions by the following
[infimum and supremum](https://en.wikipedia.org/wiki/Infimum_and_supremum) functions.  Because we derived the order on
players, we have `O < B < X`.

```haskell
supremum, infimum :: [Player] -> Player

supremum []     = O            -- The maximum function would (rightly) give an error instead.
supremum (X:ps) = X            -- Pruning takes place here - we don't look at ps.
supremum (O:ps) = supremum  ps
supremum (B:ps) = supremumB ps -- We now know that supremum ps >= B.
  where
    supremumB []     = B
    supremumB (X:ps) = X       -- Pruning takes place here too - we don't look at ps
    supremumB (_:ps) = supremumB ps
```

The definition of infimum is symmetric:

```haskell
infimum []     =  X
infimum (X:ps) = infimum  ps
infimum (O:ps) = O
infimum (B:ps) = infimumB ps
  where
    infimumB []     = B
    infimumB (O:ps) = O
    infimumB (_:ps) = infimumB ps
```

Minimax with lazy α-β-pruning. We just replace `minimum` and `maximum` with `infimum` and `supremum` in the definitions given above:

```haskell
minimax' :: Tree Grid -> Tree (Grid,Player)
minimax' (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax' (Node g ts)
   | turn g == O = Node (g, infimum  ps) ts'
   | turn g == X = Node (g, supremum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']
```

Use the game tree to compute a best move for a player in a given grid using α-β-pruning:
```haskell
bestmove' :: Grid -> Player -> Grid
bestmove' g p = head [g' | Node (g',p') _ <- ts, p' == best]
                where
                   tree = prune depth (gametree g p)
                   Node (_,best) ts = minimax' tree
```

We now perform some statistics.

The number of plays is the number of paths, which is the same as the
number of leaves:

```haskell
leavescount :: Tree a -> Int
leavescount (Node _ []) = 1
leavescount (Node _ forest) = sum [leavescount tree | tree <- forest]
```

In my teaching laptop (running on battery):

```
*Main> leavescount (gametree empty X)
255168
(14.65 secs, 11,193,202,264 bytes)
```

This means that there are `255168` valid game plays, which is slightly less than
`9!=362880`. This is because some plays end in less than `9`
moves. The actual number of valid plays is `2^6 * 3^2 * 443`, where
`443` is a prime number. However, some of these `443` plays [end up in
the same position](https://en.wikipedia.org/wiki/Tic-tac-toe).


Lazy alpha-beta pruning makes a huge difference:

```
*Main> bestmove' empty X
[[X,B,B],[B,B,B],[B,B,B]]
(2.04 secs, 1,356,556,320 bytes)
```

Why? Attend the lecture on lazy evaluation to see the answer.

## Another way to do the same thing

[Is here](tictactoe-lazily-pruned.hs). We instead keep the book code as
it is, except that we implement our own instance of the `Ord` type
class, to make the `min` and `max` (and hence `minimum` and `maximum`)
functions lazier than they are when we automatically derive them. This
actually gives code that is `50` times faster for evaluating `bestmove
empty X` and takes a fraction of a second.

## Minimax with explicit α-β-pruning

```haskell
alphabeta :: Tree Grid -> Tree (Grid,Player)
alphabeta (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
alphabeta (Node g ts)
   | turn g == O = Node (g, minimum o) (take (length o) ts')
   | turn g == X = Node (g, maximum x) (take (length x) ts')
                   where
                      ts' = map alphabeta ts
                      ps  = [p | Node (_,p) _ <- ts']
                      o = takeUntil (== O) ps
                      x = takeUntil (== X) ps

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p [] = []
takeUntil p (x : xs) | p x       = [x]
                     | otherwise = x : takeUntil p xs
```

The α-β-pruned tree is much smaller:
```
*Main> leavescount (alphabeta (gametree empty X))
19
(0.03 secs, 1,456,024 bytes)
```

Now we can see why the above lazy prunings work fast: because the search tree is much smaller.

You can replace `minimax` by `alphabeta` in the definition of
`bestmove` to get `bestmove empty X` computed in a fraction of a
second.

## Going further

 * Instead of generating game trees explicitly, you can use backtracking to
   determine the best move to play next.

 * In particular, you can use a continuation-passing-style (CPS) approach,
   with failure and success continuations.

 * The continuation-passing style amounts to using the *continuation monad*, as we explained in previous lectures on the CPS translation.

 * There is a related [selection monad](http://www.cs.bham.ac.uk/~mhe/papers/msfp2010/).

 * In particular, [there is](http://www.cs.bham.ac.uk/~mhe/papers/msfp2010/MSFP2010/haskell/index.html) a Haskell program to determine the next
   optimal move for tic-tac-toe using the selection monad, which gives a rather short program to play tic-tac-toe, and indeed any sequential game, incorporating the above lazy alpha-beta-prunning.
