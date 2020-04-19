# Assessed assignment 3

## Instructions

* If you have not yet cloned the module Gitlab repository, then do that with:
  ```shell
  git clone https://git.cs.bham.ac.uk/zeilbern/fp-learning-2019-2020.git
  ```
  If you've already cloned the repository, run
  ```shell
  git pull
  ```
  to ensure that you have the most recent version including this assignment.

* Go into the "Assignments/Assessed/Assessed3" directory and *copy* the given `Assessed3Template.hs` to a *new* file `Assessed3.hs`.
  Work on that file to produce your solution and then submit it to Canvas.

  It is important to copy the file as explained, since any future `git pull` may overwrite the template. Hence don't work directly on the template.

* If you work inside another directory besides the git repository, you will also need to copy the file `Types.hs`, which includes type definitions that are imported by the template file.
  Any data or types that you define yourself should be in `Assessed2.hs`, but the ones defined in the assignment are and should stay in `Types.hs`.
  You should **not** modify the `Types.hs` file.

* You will also probably want to copy the file `DomViz.hs`, which contains some visualization procedures based on the [diagrams library](https://archives.haskell.org/projects.haskell.org/diagrams/).
  If you don't already have this library installed on your personal machine, installing it could be as simple as running
  ```
  cabal install diagrams
  ```
  from a terminal.
  On the other hand, if that doesn't work and you have trouble installing the library, your options are to:

  1. try to get help on Slack;
  2. use the lab machines, which already have the diagrams library installed;
  3. comment out the `import DomViz` statement from your solution file.

  Of course, option (3) sadly means that you won't have access to the visualization procedures on your machine.
  They are intended to make the programming task more enjoyable and to help you with debugging, but strictly speaking they are not needed to complete the assignment.

* Because the marking is automatic, we need your submission to be in the correct format, and compile and type check without errors.
  A "presubmit" script will be released, which you need to run before you submit your assignment on Canvas.
  **Submissions that don't pass the presubmit test won't qualify for marks.**

* Be aware that:

  * Your solutions must work with GHC 8.6.5. To use GHC 8.6.5 on a lab machine, see [HardwareAndSoftware.md](../../Resources/HardwareAndSoftware.md).


  * If you wish to import modules, then you may only import libraries from [the standard library](http://hackage.haskell.org/package/base). Additionally, all modules you import must be "Safe" on Hackage.

  * Some of the questions are harder than others, and these are indicated according to the rubric below.
    IT IS OKAY IF YOU DON'T FIND COMPLETE SOLUTIONS TO ALL THE QUESTIONS.

## Background

### The game of Domineering

**Domineering** is a two-player strategy game that can be played over any shape board composed out of some collection of squares.
Players alternate taking turns placing dominoes on the board to cover up a pair of squares, with one player (which we will refer to as "H") always placing them horizontally, and the other player ("V") always placing them vertically.
The first player who cannot make a move loses.

For example, below is the full record of a game played on a 4x4 board, with H playing first in round 1 and winning in round 8, since V has nowhere left to place a domino.

![board4x4-1](diagrams/board4x4-1.svg)
![board4x4-2](diagrams/board4x4-2.svg)
![board4x4-3](diagrams/board4x4-3.svg)
![board4x4-4](diagrams/board4x4-4.svg)
![board4x4-5](diagrams/board4x4-5.svg)
![board4x4-6](diagrams/board4x4-6.svg)
![board4x4-7](diagrams/board4x4-7.svg)
![board4x4-8](diagrams/board4x4-8.svg)

Here is another example of a game on a 5x5 board, where this time H loses in round 13.
(We give only the abbreviated transcript of the game, showing the final position and the labelling of the moves by round.)

![board5x5](diagrams/board5x5.svg)

A Domineering board does not necessarily have to be square.
For example, here we see the transcript of a game on a board with two columns and nine rows.

![board2x9](diagrams/board2x9.svg)

Nor does the board have to be contiguous.
Here are some games played on crosshatch-shaped boards of different sizes:

![hatch2](diagrams/hatch2.svg)
&nbsp;&nbsp;&nbsp;&nbsp;
![hatch3](diagrams/hatch3.svg)

### Recent developments

This year, one of the most remarkable games in Domineering history was played between Alpha Dom and Lee Sedom, with Alpha Dom claiming victory in round 28.
We show the transcript of the game below, together with another one demonstrating Alpha Dom's mastery over a lower-skilled opponent.

*Alpha Dom vs Lee Sedom* (2019):

![alphadom-vs-leesedom](diagrams/alphadom-vs-leesedom.svg) 

*Alpha Dom vs Ran Dom* (2019):

![alphadom-vs-random](diagrams/alphadom-vs-random.svg) 

### Representing the game

In this assignment, you will be tasked with writing various functions related to playing the game of Domineering.
Some of these will be very similar to analogous functions covered in the [Tic Tac Toe](../../../LectureNotes/tictactoe.md) lectures and in Chapter 11 of Hutton, so you may want to review that material as preparation for this assignment.
In a few places we will make slightly different design decisions, which will be signaled.

Let's begin by establishing the basic representation for players, moves, and boards.

```haskell
data Player = H | V
  deriving (Eq, Ord, Show)

type Cell = (Int, Int)

data Board = Board { turn :: Player, free :: [Cell], hist :: [Cell] }
  deriving (Eq, Show)
```

A board is represented by a record containing three fields:
* `turn` specifies the player whose turn it is to play;
* `free` specifies the list of unoccupied squares;
* `hist` specifies the list of previous moves played over the course of the game, *in reverse order.*

We do not make any type distinction between "squares" and "moves": both are represented by values of type `Cell`, this being a synonym for the type `(Int, Int)` of two-dimensional integer coordinates.
However, when we indicate a move by a cell, this is always interpreted as referring to the coordinate of the *lower left corner* of the domino.

For example, consider this board:

![board4x4-3](diagrams/board4x4-3.svg)

```haskell
board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }
```

Here we have assigned coordinates to the cells ranging from (1,1) for the lower left square to (4,4) for the upper right square.
In round 1, H plays a horizontal domino at (2,1), which also occupies the cell (3,1).
In round 2, V plays a vertical domino at (1,3), which also occupies the cell (1,4).
(Note our visualization conventions are the same as we used for the [Game of Life](../../../LectureNotes/Life.md), with the x coordinate increasing as we move rightwards across the board, and with the y coordinate increasing as we move upwards across the board.)

The following utility functions will be useful:
```haskell
-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b
```

You may also wish to experiment using the following functions that construct boards of different shapes.
```haskell
-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]
```

### Representing game trees

We will represent game trees using rose trees just as we did for [Tic Tac Toe](../../../LectureNotes/tictactoe.md), although for convenience we will import the type from `Data.Tree`.
Officially, `Tree a` is defined in `Data.Tree` as a record type in mutual definition with `Forest a`,
```hs
type Forest a = [Tree a]
data Tree a = Node {rootLabel :: a, subForest :: Forest a}
```
but if you prefer you can simply ignore the field labels and the type synonym, treating `Tree` as though it were defined by our usual definition:
```hs
data Tree a = Node a [Tree a]
```

### Visualization

The module [`DomViz`](DomViz.hs) exports a pair of functions
```hs
boardSVG :: Board -> String -> IO ()
gametreeSVG :: Tree Board -> String -> IO ()
```
for visualizing games of Domineering, implemented using the [diagrams library](https://archives.haskell.org/projects.haskell.org/diagrams/).
Given a board `b` and a string `basename`, the command `boardSVG b basename` creates an SVG file named `basename ++ ".svg"` containing a graphical representation of `b`.
For example, visualizations for the two recent victories by Alpha Dom that we mentioned above can be created by running the commands
```hs
> boardSVG alphaDom_vs_LeeSedom "alphadom-vs-leesedom"
> boardSVG alphaDom_vs_RanDom "alphadom-vs-random"
```
after introducing the following definitions:
```haskell
alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }
```

Similarly, given a game tree `t` and a string `basename`, the command `gametreeSVG t basename` creates an SVG file named `basename ++ ".svg"` containing a graphical representation of `t`.
We will give a demonstration of this below.

The `DomViz` module also exports a related pair of functions:
```hs
boardSVG' :: Double -> Board -> String -> IO ()
gametreeSVG' :: Double -> Tree (Board, String) -> String -> IO ()
```
Both take an extra scale factor as first input, in case you want to grow or shrink the SVG graphics, and `gametreeSVG'` takes a game tree where each node has been annotated with a label.
Note that `boardSVG` and `gametreeSVG` are defined by
`boardSVG = boardSVG' 1`
and `gametreeSVG = gametreeSVG' 1 . fmap (\b -> (b,""))`
respectively.

## Questions

This assignment is worth a total of 60 marks.
Harder questions are indicated with extra stars.


1. The first group of questions involves determining what moves are legal, performing legal moves, and "undoing" moves to recover the full record of a game.

   * **[5 marks]**
     Write a function
     ```haskell
     legalMoves :: Player -> Board -> [Cell]
     legalMoves = undefined
     ```
     that returns the list of all legal moves for a player on a given board.  Note that here we mean the legal moves for either player, *regardless* of whether it is that player's turn to play.
     ```hs
     > legalMoves H board4x4_3
     [(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4)]
     > legalMoves V board4x4_3
     [(1,1),(2,2),(2,3),(3,2),(3,3),(4,1),(4,2),(4,3)]
     ```

   * **[5 marks]**
     Write a function
     ```haskell
     moveLegal :: Board -> Cell -> Board
     moveLegal = undefined
     ```
     that takes a board and a *legal* move for the player whose turn it is to play, and returns the new board resulting from executing that play.
     If the move is actually illegal for the current player, then the behavior of `moveLegal` is unspecified.
     ```hs
     > moveLegal board4x4_3 (2,3)
     Board {turn = V, free = [(1,1),(1,2),(2,2),(2,4),(3,2),(3,4),(4,1),(4,2),(4,3),(4,4)], hist = [(2,3),(1,3),(2,1)]}
     ```
   
   * **[10 marks] (★)**
     Write a function
     ```haskell
     replay :: Board -> [Board]
     replay = undefined
     ```
     that takes a board with some possibly non-empty history of moves, and returns the full record of the game leading up to that position, starting from an initially empty board.
     ```hs
     > replay board4x4_3
     [Board {turn = H, free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4),(1,3),(1,4),(2,1),(3,1)], hist = []},Board {turn = V, free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4),(1,3),(1,4)], hist = [(2,1)]},Board {turn = H, free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)], hist = [(1,3),(2,1)]}]
     ```
2. The second group of questions are about playing optimally using minimax search on game trees.

   Now that you've implemented `legalMoves` and `moveLegal`, game trees for Domineering can be computed almost exactly as we did for Tic Tac Toe:
   ```haskell
   gametree :: Board -> Tree Board
   gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]
   ```
   If you like, you can visualize such trees using the `gametreeSVG` function mentioned above.
   For example, the command
   ```hs
   gametreeSVG (gametree (board 3 3)) "board3x3-full"
   ```
   will generate a rendering of the [full game tree](diagrams/board3x3-full.svg) for the 3x3 board with H to start.
   However, you will find that these game trees can grow quite large!
   For example, the full game tree for the 5x5 board has 2103584601 nodes.
   Pruning of game trees can be implemented exactly as before:
   ```haskell
   prune :: Int -> Tree a -> Tree a
   prune 0 (Node x _)  = Node x []
   prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]
   ```
   For example, click here to see the [depth 2 pruned game tree](diagrams/board3x3-depth2.svg) for the 3x3 board.

   Minimax is parameterised by a *scoring function* that assigns values to the leaves of a game tree.
   We will introduce the following type for scores:
   ```haskell
   data Score = Win Player | Heu Int  deriving (Show,Eq)
   ```
   The idea is that either we already know that some player `p` has a winning strategy from a given board, in which case we can assign it the score `Win p`, or else we can assign it some heuristic integer value `Heu x`, with negative values favoring H and positive values favoring V.
   Values of type `Score` are therefore naturally ordered, with
   ```
   Win H <= Heu x <= Win V
   ```
   for all `x`, and
   `Heu x <= Heu y`
   just in case `x <= y`.
   There is a corresponding instance of `Ord Score` provided in `Types.hs`.

   * **[5 marks]**
     Write a scoring function
     ```haskell
     score :: Board -> Score
     score = undefined
     ```
     implementing the following heuristic: if it is `p`'s turn to play, return a win for `p`'s opponent if `p` has no legal moves, and otherwise return a heuristic value based on the formula
     ```
     #(legal moves for V) - #(legal moves for H) - sign(p)
     ```
     where `sign(H) = -1` and `sign(V) = 1`.
     
     Example:
     ```hs
     > score board4x4_3
     Heu 2
     ```

   * **[5 marks]**
     Write a function
     ```haskell
     minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
     minimax = undefined
     ```
     that annotates every node of a game tree with a minimax score, computed relative to an arbitrary scoring function for the leaves (first argument).
     Your implementation will probably be very similar to the implementation of `minimax` we wrote for [Tic Tac Toe](../../../LectureNotes/tictactoe.md), except that it takes the extra scoring function as a parameter.
     It should leave the tree untouched other than annotating each node with a score (in other words, it should satisfy `fmap fst (minimax sfn t) = t`).

     Click here to see the [minimax scoring of the depth 2 pruned game tree](diagrams/board3x3-depth2-minimax.svg) for the 3x3 board, using the scoring function `score` from above.
     This diagram was generated with the following command:
     ```hs
     gametreeSVG' 1 (fmap (\(b,v) -> (b,show v)) $ minimax score $ prune 2 $ gametree (board 3 3)) "board3x3-depth2-minimax"
     ```

   * **[10 marks]**
     Write a function
     ```haskell
     bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
     bestmoves = undefined
     ```
     which takes a depth `d` and a scoring function `scorefn` as parameters, and defines a function from boards to lists of *optimal* moves.
     Each move returned should be optimal in the sense that it has the best minimax score for the current player, relative to the scoring function `scorefn` applied to the game tree pruned to depth `d`.
     Moreover, the list returned should be the *complete* list of optimal moves, although they can be returned in any order.

     Take notice that an optimal move is not necessarily a *winning* move, in the case where the player has no better options.
     Your implementation will probably be very similar to the function `bestmove` we wrote using `minimax` for Tic Tac Toe, except that we are asking you to return the list of *all* optimal moves.
     
     Examples:
     ```hs
     > bestmoves 4 score (board 3 3)
     [(1,2),(2,2)]
     > bestmoves 4 score (moveLegal (board 3 3) (1,2))
     [(3,1),(3,2)]
     ```
     
3. Now that you've implemented `bestmoves`, we can combine it with an operation `chooseSafe` implemented using our old friend the `PickingMonad` to get a player who selects optimal moves at random, and compare it against a player who selects legal moves at random.
   ```haskell
   chooseSafe :: PickingMonad m => [a] -> m (Maybe a)
   chooseSafe [] = return Nothing
   chooseSafe xs = do
     i <- pick 0 (length xs - 1)
     return (Just (xs !! i))
   
   randomBestPlay :: PickingMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
   randomBestPlay d sfn = chooseSafe . bestmoves d sfn
   randomPlay :: PickingMonad m => Board -> m (Maybe Cell)
   randomPlay b = chooseSafe (legalMoves (turn b) b)
   ```
   Note we have to use `chooseSafe` here, which returns a `Maybe` type, since it is possible that the list of optimal/legal moves is empty.

   * **[10 marks]**
     Write a function
     ```haskell
     runGame :: PickingMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
     runGame = undefined
     ```
     that takes two players `playH` and `playV` of type `Board -> m (Maybe Cell)`, and plays them off against each other starting from some initial board to produce a final board.
     It should run in any `PickingMonad`, and it should stop and return the current board as soon as either player gives up (i.e., returns `Nothing`) or makes an illegal move.

     For example, here are three games on a 4x4 board produced by setting `randomBestPlay 8 score` vs `randomBestPlay 8 score`, `randomPlay` vs `randomBestPlay 8 score`, and `randomPlay` vs `randomPlay`, respectively.
     
     ![randombest-vs-randombest](diagrams/randombest-vs-randombest.svg)
     &nbsp;&nbsp;&nbsp;&nbsp;
     ![random-vs-randombest](diagrams/random-vs-randombest.svg)
     &nbsp;&nbsp;&nbsp;&nbsp;
     ![random-vs-random](diagrams/random-vs-random.svg)

4. Now that you have a good supply of Domineering players, you'd like some more interesting boards!
   * **[10 marks] (★)**
     Write a function
     ```haskell
     carpets :: [Board]
     carpets = undefined
     ```
     that generates an infinite sequence of empty boards, corresponding to the successive iterations of the [Sierpinski carpet](https://en.wikipedia.org/wiki/Sierpinski_carpet), where player H starts on even-numbered carpets and V starts on odd-numbered carpets.
     Here's what the first few look like (we omit `carpets !! 0`, which is a pretty boring board):

     **`carpets !! 1`** = ![carpet1](diagrams/carpet1.svg)

     **`carpets !! 2`** = ![carpet2](diagrams/carpet2.svg)

     **`carpets !! 3`** = ![carpet3](diagrams/carpet3.svg)


## A challenge problem, and the official AFP Domineering Challenge

If you've already finished the assignment and want to try a harder problem, you can think about how to improve your minimax-based player by implementing [alpha-beta pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning) (see first note below).
Independently of that, you can try to improve your player in other ways, and enter it as a contestant in the official AFP Domineering Challenge tournament (see second note below)!

### On alpha-beta pruning

As we mentioned, game trees in Domineering can get very large very quickly, with the full game tree for the 5x5 board already containing over two billion nodes.
An important general technique for reducing the size of game trees is called [alpha-beta pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning).
The basic idea is that very often, large subtrees can be automatically eliminated from consideration (a.k.a. "pruned"), since they cannot possibly contribute to an optimal strategy for the player starting at the root.

You can find a basic description of alpha-beta pruning at Wikipedia, including [pseudocode](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning#Pseudocode).
One way to think about this in Haskell is to refine the type of the `minimax` function, so that it returns a slightly more sophisticated kind of scoring:
```
data ABScore = Exact Score | Bound Score

alphabeta :: (Board -> Score) -> Tree Board -> Tree (Board, Maybe ABScore)
alphabeta = undefined
```
The idea is that given a scoring function, `alphabeta` annotates every board `b` of the game tree with one of three possible values:

1. a score of `Just (Exact v)` indicates that the optimal strategy for the player starting at `b` has value `v`;
2. a score of `Just (Bound v)` indicates that the optimal strategy for the player starting at `b` has value *at least as good as* `v`, although the player may have an even better strategy;
3. a score of `Nothing` indicates nothing, and is used to prune the node `b` when we don't care about its value.

Click here to see the [alpha-beta scoring of the depth 2 game tree](diagrams/board3x3-alpha2.svg) for the 3x3 board, where the nodes have been visited from left to right, and where we've indicated pruned nodes with an "X".
Here's another, smaller version where the pruned nodes have been removed altogether:

![board3x3-alpha2-pruned.svg](diagrams/board3x3-alpha2-pruned.svg)

Observe that some of the nodes are marked with lower bounds, e.g., `≥Heu 2` and `≥Heu 0`.
In both these two cases, the true minimax value of the nodes is actually higher, namely `Win V`, but this is irrelevant to the minimax value of the root, since we can already tell that playing either of these moves will lead to a worse position (for player `H`) than playing their sibling node marked `Heu (-2)`.

With alpha-beta pruning, often it's the case that increasing the overall pruning depth can actually decrease the size of the tree.
For example, here is the left-to-right alpha-beta pruning of the full game tree for the 3x3 board:

![board3x3-alpha4-pruned.svg](diagrams/board3x3-alpha4-pruned.svg)

Since we can already tell that `H` has a winning strategy, there is no point (from `H`'s perspective) in further exploring the game tree.

For the 4x4 board, the size of the game tree is reduced from 65081 nodes (without alpha-beta pruning) to 1362 (with pruning), and for the 5x5 board, the size goes down from 2103584601 nodes to a much more manageable 3634155 nodes!

There are many other possibilities for improving the efficiency of minimax search.
Memoization of previously explored nodes can significantly reduce the time needed to explore the game tree, as can exploiting symmetries in the problem.
Also, alpha-beta pruning can be improved by using some more rational criterion to guide the order in which the nodes are visited, rather than simply scanning them from left to right, so as to increase the chances of finding good strategies early on.

You can find out more about strategies specific to Domineering at the [Wikipedia page](https://en.wikipedia.org/wiki/Domineering), as well as at [this site](http://webdocs.cs.ualberta.ca/~games/domineering/) by Nathan Bullock.


### The tournament

To enter the official AFP Domineering Challenge, you need to implement one additional function in your solution file, containing the definition of a *strategy function*:
```hs
myStrategy :: PickingMonad m => Board -> m (Maybe Cell)
myStrategy = undefined
```

You should add this to your `Assessed3.hs` file, and then submit it as a separate submission on Canvas for the Domineering Challenge.
You can define `myStrategy` in any way you like.
(You will probably want to test it before you submit!
The function `simulate` you wrote for [Assessed2](../Assessed2/README.md) may be useful...)

The Challenge will consist of a round-robin tournament pitting strategy functions against each other, followed by a championship match between the top two contestants.
All tournament play will take place on `carpets !! 2`.

During the round-robin tournament, every player faces every other player exactly once*.
A match consists of <strike>eight*</strike>64 games, with both players playing an equal number of times as H and as V.
Over the course of a single game, each player can use up to 8 seconds of clock time (over the total of their moves, not for each individual move); if a player times out, the other player wins the game by default, and the match continues.
Once <strike>eight*</strike>64 games have been played, the winner of the match is the player who won the highest total number of games, or else the match ends in a draw if both players won the same number of games.

(*We reserve the right to change the total number of matches and the number of games per match, depending on the number of contestants.  However, we will not change the time limit of 8 seconds per player per game, as measured on the lab machines. **Update:** we decided to increase the number of games per match to 64.)

After the end of the round-robin tournament, each player will be assigned a ranking according to their total number of points, where each win counts for 8 points and each draw counts for 4 points.
The two highest ranking players will then face each other in a championship match.

The championship match will consist of <strike>16</strike>64 games, played under the same conditions.
In the event that the championship match ends in a draw, to break the tie we will pass to `carpets !! 3`, then `carpets !! 4`, and so on.
Just kidding.
In the event of a tie we will award two first place trophies.

### Results

There were seven players who entered in the Domineering Challenge: AD, AG, BT, CL, GD, KI, and RD.
One player (CL) was eliminated from competition due to not returning any moves on `carpets !! 2` within the 8-second time limit.
The round-robin tournament was played between the remaining six players.

Click here to see the [results](http://noamz.org/DC2019-roundrobin/results.txt) of the round-robin tournament.

Complete record of individual matchups:

* **KI vs AD**
  * H = KI, V = AD:
  [1](http://noamz.org/DC2019-roundrobin/KI-vs-AD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/KI-vs-AD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/KI-vs-AD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/KI-vs-AD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/KI-vs-AD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/KI-vs-AD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/KI-vs-AD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/KI-vs-AD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/KI-vs-AD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/KI-vs-AD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/KI-vs-AD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/KI-vs-AD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/KI-vs-AD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/KI-vs-AD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/KI-vs-AD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/KI-vs-AD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/KI-vs-AD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/KI-vs-AD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/KI-vs-AD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/KI-vs-AD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/KI-vs-AD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/KI-vs-AD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/KI-vs-AD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/KI-vs-AD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/KI-vs-AD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/KI-vs-AD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/KI-vs-AD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/KI-vs-AD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/KI-vs-AD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/KI-vs-AD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/KI-vs-AD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/KI-vs-AD-32.svg)
  * H = AD, V = KI:
  [1](http://noamz.org/DC2019-roundrobin/AD-vs-KI-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AD-vs-KI-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AD-vs-KI-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AD-vs-KI-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AD-vs-KI-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AD-vs-KI-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AD-vs-KI-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AD-vs-KI-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AD-vs-KI-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AD-vs-KI-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AD-vs-KI-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AD-vs-KI-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AD-vs-KI-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AD-vs-KI-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AD-vs-KI-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AD-vs-KI-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AD-vs-KI-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AD-vs-KI-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AD-vs-KI-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AD-vs-KI-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AD-vs-KI-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AD-vs-KI-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AD-vs-KI-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AD-vs-KI-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AD-vs-KI-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AD-vs-KI-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AD-vs-KI-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AD-vs-KI-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AD-vs-KI-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AD-vs-KI-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AD-vs-KI-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AD-vs-KI-32.svg)
  
* **AG vs AD**
  * H = AG, V = AD:
  [1](http://noamz.org/DC2019-roundrobin/AG-vs-AD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AG-vs-AD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AG-vs-AD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AG-vs-AD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AG-vs-AD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AG-vs-AD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AG-vs-AD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AG-vs-AD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AG-vs-AD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AG-vs-AD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AG-vs-AD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AG-vs-AD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AG-vs-AD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AG-vs-AD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AG-vs-AD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AG-vs-AD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AG-vs-AD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AG-vs-AD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AG-vs-AD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AG-vs-AD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AG-vs-AD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AG-vs-AD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AG-vs-AD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AG-vs-AD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AG-vs-AD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AG-vs-AD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AG-vs-AD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AG-vs-AD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AG-vs-AD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AG-vs-AD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AG-vs-AD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AG-vs-AD-32.svg)
  * H = AD, V = AG:
  [1](http://noamz.org/DC2019-roundrobin/AD-vs-AG-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AD-vs-AG-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AD-vs-AG-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AD-vs-AG-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AD-vs-AG-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AD-vs-AG-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AD-vs-AG-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AD-vs-AG-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AD-vs-AG-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AD-vs-AG-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AD-vs-AG-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AD-vs-AG-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AD-vs-AG-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AD-vs-AG-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AD-vs-AG-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AD-vs-AG-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AD-vs-AG-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AD-vs-AG-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AD-vs-AG-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AD-vs-AG-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AD-vs-AG-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AD-vs-AG-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AD-vs-AG-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AD-vs-AG-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AD-vs-AG-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AD-vs-AG-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AD-vs-AG-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AD-vs-AG-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AD-vs-AG-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AD-vs-AG-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AD-vs-AG-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AD-vs-AG-32.svg)

* **RD vs AD**
  * H = RD, V = AD:
  [1](http://noamz.org/DC2019-roundrobin/RD-vs-AD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/RD-vs-AD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/RD-vs-AD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/RD-vs-AD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/RD-vs-AD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/RD-vs-AD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/RD-vs-AD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/RD-vs-AD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/RD-vs-AD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/RD-vs-AD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/RD-vs-AD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/RD-vs-AD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/RD-vs-AD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/RD-vs-AD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/RD-vs-AD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/RD-vs-AD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/RD-vs-AD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/RD-vs-AD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/RD-vs-AD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/RD-vs-AD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/RD-vs-AD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/RD-vs-AD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/RD-vs-AD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/RD-vs-AD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/RD-vs-AD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/RD-vs-AD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/RD-vs-AD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/RD-vs-AD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/RD-vs-AD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/RD-vs-AD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/RD-vs-AD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/RD-vs-AD-32.svg)
  * H = AD, V = RD:
  [1](http://noamz.org/DC2019-roundrobin/AD-vs-RD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AD-vs-RD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AD-vs-RD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AD-vs-RD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AD-vs-RD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AD-vs-RD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AD-vs-RD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AD-vs-RD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AD-vs-RD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AD-vs-RD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AD-vs-RD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AD-vs-RD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AD-vs-RD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AD-vs-RD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AD-vs-RD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AD-vs-RD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AD-vs-RD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AD-vs-RD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AD-vs-RD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AD-vs-RD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AD-vs-RD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AD-vs-RD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AD-vs-RD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AD-vs-RD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AD-vs-RD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AD-vs-RD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AD-vs-RD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AD-vs-RD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AD-vs-RD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AD-vs-RD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AD-vs-RD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AD-vs-RD-32.svg)

* **AG vs RD**
  * H = AG, V = RD:
  [1](http://noamz.org/DC2019-roundrobin/AG-vs-RD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AG-vs-RD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AG-vs-RD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AG-vs-RD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AG-vs-RD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AG-vs-RD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AG-vs-RD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AG-vs-RD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AG-vs-RD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AG-vs-RD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AG-vs-RD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AG-vs-RD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AG-vs-RD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AG-vs-RD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AG-vs-RD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AG-vs-RD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AG-vs-RD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AG-vs-RD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AG-vs-RD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AG-vs-RD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AG-vs-RD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AG-vs-RD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AG-vs-RD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AG-vs-RD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AG-vs-RD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AG-vs-RD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AG-vs-RD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AG-vs-RD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AG-vs-RD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AG-vs-RD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AG-vs-RD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AG-vs-RD-32.svg)
  * H = RD, V = AG:
  [1](http://noamz.org/DC2019-roundrobin/RD-vs-AG-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/RD-vs-AG-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/RD-vs-AG-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/RD-vs-AG-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/RD-vs-AG-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/RD-vs-AG-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/RD-vs-AG-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/RD-vs-AG-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/RD-vs-AG-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/RD-vs-AG-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/RD-vs-AG-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/RD-vs-AG-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/RD-vs-AG-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/RD-vs-AG-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/RD-vs-AG-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/RD-vs-AG-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/RD-vs-AG-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/RD-vs-AG-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/RD-vs-AG-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/RD-vs-AG-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/RD-vs-AG-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/RD-vs-AG-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/RD-vs-AG-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/RD-vs-AG-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/RD-vs-AG-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/RD-vs-AG-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/RD-vs-AG-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/RD-vs-AG-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/RD-vs-AG-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/RD-vs-AG-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/RD-vs-AG-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/RD-vs-AG-32.svg)

* **AG vs GD**
  * H = AG, V = GD:
  [1](http://noamz.org/DC2019-roundrobin/AG-vs-GD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AG-vs-GD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AG-vs-GD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AG-vs-GD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AG-vs-GD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AG-vs-GD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AG-vs-GD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AG-vs-GD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AG-vs-GD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AG-vs-GD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AG-vs-GD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AG-vs-GD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AG-vs-GD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AG-vs-GD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AG-vs-GD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AG-vs-GD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AG-vs-GD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AG-vs-GD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AG-vs-GD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AG-vs-GD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AG-vs-GD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AG-vs-GD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AG-vs-GD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AG-vs-GD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AG-vs-GD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AG-vs-GD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AG-vs-GD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AG-vs-GD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AG-vs-GD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AG-vs-GD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AG-vs-GD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AG-vs-GD-32.svg)
  * H = GD, V = AG:
  [1](http://noamz.org/DC2019-roundrobin/GD-vs-AG-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/GD-vs-AG-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/GD-vs-AG-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/GD-vs-AG-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/GD-vs-AG-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/GD-vs-AG-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/GD-vs-AG-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/GD-vs-AG-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/GD-vs-AG-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/GD-vs-AG-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/GD-vs-AG-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/GD-vs-AG-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/GD-vs-AG-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/GD-vs-AG-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/GD-vs-AG-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/GD-vs-AG-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/GD-vs-AG-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/GD-vs-AG-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/GD-vs-AG-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/GD-vs-AG-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/GD-vs-AG-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/GD-vs-AG-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/GD-vs-AG-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/GD-vs-AG-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/GD-vs-AG-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/GD-vs-AG-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/GD-vs-AG-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/GD-vs-AG-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/GD-vs-AG-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/GD-vs-AG-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/GD-vs-AG-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/GD-vs-AG-32.svg)

* **BT vs AD**
  * H = BT, V = AD:
  [1](http://noamz.org/DC2019-roundrobin/BT-vs-AD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/BT-vs-AD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/BT-vs-AD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/BT-vs-AD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/BT-vs-AD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/BT-vs-AD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/BT-vs-AD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/BT-vs-AD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/BT-vs-AD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/BT-vs-AD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/BT-vs-AD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/BT-vs-AD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/BT-vs-AD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/BT-vs-AD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/BT-vs-AD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/BT-vs-AD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/BT-vs-AD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/BT-vs-AD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/BT-vs-AD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/BT-vs-AD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/BT-vs-AD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/BT-vs-AD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/BT-vs-AD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/BT-vs-AD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/BT-vs-AD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/BT-vs-AD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/BT-vs-AD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/BT-vs-AD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/BT-vs-AD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/BT-vs-AD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/BT-vs-AD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/BT-vs-AD-32.svg)
  * H = AD, V = BT:
  [1](http://noamz.org/DC2019-roundrobin/AD-vs-BT-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AD-vs-BT-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AD-vs-BT-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AD-vs-BT-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AD-vs-BT-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AD-vs-BT-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AD-vs-BT-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AD-vs-BT-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AD-vs-BT-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AD-vs-BT-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AD-vs-BT-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AD-vs-BT-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AD-vs-BT-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AD-vs-BT-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AD-vs-BT-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AD-vs-BT-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AD-vs-BT-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AD-vs-BT-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AD-vs-BT-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AD-vs-BT-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AD-vs-BT-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AD-vs-BT-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AD-vs-BT-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AD-vs-BT-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AD-vs-BT-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AD-vs-BT-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AD-vs-BT-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AD-vs-BT-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AD-vs-BT-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AD-vs-BT-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AD-vs-BT-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AD-vs-BT-32.svg)

* **BT vs KI**
  * H = BT, V = KI:
  [1](http://noamz.org/DC2019-roundrobin/BT-vs-KI-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/BT-vs-KI-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/BT-vs-KI-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/BT-vs-KI-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/BT-vs-KI-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/BT-vs-KI-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/BT-vs-KI-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/BT-vs-KI-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/BT-vs-KI-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/BT-vs-KI-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/BT-vs-KI-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/BT-vs-KI-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/BT-vs-KI-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/BT-vs-KI-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/BT-vs-KI-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/BT-vs-KI-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/BT-vs-KI-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/BT-vs-KI-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/BT-vs-KI-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/BT-vs-KI-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/BT-vs-KI-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/BT-vs-KI-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/BT-vs-KI-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/BT-vs-KI-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/BT-vs-KI-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/BT-vs-KI-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/BT-vs-KI-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/BT-vs-KI-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/BT-vs-KI-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/BT-vs-KI-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/BT-vs-KI-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/BT-vs-KI-32.svg)
  * H = KI, V = BT:
  [1](http://noamz.org/DC2019-roundrobin/KI-vs-BT-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/KI-vs-BT-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/KI-vs-BT-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/KI-vs-BT-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/KI-vs-BT-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/KI-vs-BT-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/KI-vs-BT-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/KI-vs-BT-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/KI-vs-BT-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/KI-vs-BT-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/KI-vs-BT-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/KI-vs-BT-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/KI-vs-BT-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/KI-vs-BT-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/KI-vs-BT-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/KI-vs-BT-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/KI-vs-BT-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/KI-vs-BT-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/KI-vs-BT-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/KI-vs-BT-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/KI-vs-BT-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/KI-vs-BT-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/KI-vs-BT-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/KI-vs-BT-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/KI-vs-BT-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/KI-vs-BT-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/KI-vs-BT-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/KI-vs-BT-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/KI-vs-BT-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/KI-vs-BT-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/KI-vs-BT-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/KI-vs-BT-32.svg)

* **BT vs RD**
  * H = BT, V = RD:
  [1](http://noamz.org/DC2019-roundrobin/BT-vs-RD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/BT-vs-RD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/BT-vs-RD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/BT-vs-RD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/BT-vs-RD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/BT-vs-RD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/BT-vs-RD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/BT-vs-RD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/BT-vs-RD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/BT-vs-RD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/BT-vs-RD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/BT-vs-RD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/BT-vs-RD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/BT-vs-RD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/BT-vs-RD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/BT-vs-RD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/BT-vs-RD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/BT-vs-RD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/BT-vs-RD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/BT-vs-RD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/BT-vs-RD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/BT-vs-RD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/BT-vs-RD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/BT-vs-RD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/BT-vs-RD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/BT-vs-RD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/BT-vs-RD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/BT-vs-RD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/BT-vs-RD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/BT-vs-RD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/BT-vs-RD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/BT-vs-RD-32.svg)
  * H = RD, V = BT:
  [1](http://noamz.org/DC2019-roundrobin/RD-vs-BT-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/RD-vs-BT-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/RD-vs-BT-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/RD-vs-BT-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/RD-vs-BT-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/RD-vs-BT-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/RD-vs-BT-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/RD-vs-BT-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/RD-vs-BT-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/RD-vs-BT-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/RD-vs-BT-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/RD-vs-BT-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/RD-vs-BT-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/RD-vs-BT-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/RD-vs-BT-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/RD-vs-BT-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/RD-vs-BT-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/RD-vs-BT-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/RD-vs-BT-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/RD-vs-BT-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/RD-vs-BT-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/RD-vs-BT-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/RD-vs-BT-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/RD-vs-BT-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/RD-vs-BT-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/RD-vs-BT-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/RD-vs-BT-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/RD-vs-BT-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/RD-vs-BT-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/RD-vs-BT-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/RD-vs-BT-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/RD-vs-BT-32.svg)

* **AD vs GD**
  * H = AD, V = GD:
  [1](http://noamz.org/DC2019-roundrobin/AD-vs-GD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AD-vs-GD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AD-vs-GD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AD-vs-GD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AD-vs-GD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AD-vs-GD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AD-vs-GD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AD-vs-GD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AD-vs-GD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AD-vs-GD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AD-vs-GD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AD-vs-GD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AD-vs-GD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AD-vs-GD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AD-vs-GD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AD-vs-GD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AD-vs-GD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AD-vs-GD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AD-vs-GD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AD-vs-GD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AD-vs-GD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AD-vs-GD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AD-vs-GD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AD-vs-GD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AD-vs-GD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AD-vs-GD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AD-vs-GD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AD-vs-GD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AD-vs-GD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AD-vs-GD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AD-vs-GD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AD-vs-GD-32.svg)
  * H = GD, V = AD:
  [1](http://noamz.org/DC2019-roundrobin/GD-vs-AD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/GD-vs-AD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/GD-vs-AD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/GD-vs-AD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/GD-vs-AD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/GD-vs-AD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/GD-vs-AD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/GD-vs-AD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/GD-vs-AD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/GD-vs-AD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/GD-vs-AD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/GD-vs-AD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/GD-vs-AD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/GD-vs-AD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/GD-vs-AD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/GD-vs-AD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/GD-vs-AD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/GD-vs-AD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/GD-vs-AD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/GD-vs-AD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/GD-vs-AD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/GD-vs-AD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/GD-vs-AD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/GD-vs-AD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/GD-vs-AD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/GD-vs-AD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/GD-vs-AD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/GD-vs-AD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/GD-vs-AD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/GD-vs-AD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/GD-vs-AD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/GD-vs-AD-32.svg)

* **KI vs GD**
  * H = KI, V = GD:
  [1](http://noamz.org/DC2019-roundrobin/KI-vs-GD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/KI-vs-GD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/KI-vs-GD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/KI-vs-GD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/KI-vs-GD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/KI-vs-GD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/KI-vs-GD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/KI-vs-GD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/KI-vs-GD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/KI-vs-GD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/KI-vs-GD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/KI-vs-GD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/KI-vs-GD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/KI-vs-GD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/KI-vs-GD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/KI-vs-GD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/KI-vs-GD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/KI-vs-GD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/KI-vs-GD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/KI-vs-GD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/KI-vs-GD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/KI-vs-GD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/KI-vs-GD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/KI-vs-GD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/KI-vs-GD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/KI-vs-GD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/KI-vs-GD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/KI-vs-GD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/KI-vs-GD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/KI-vs-GD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/KI-vs-GD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/KI-vs-GD-32.svg)
  * H = GD, V = KI:
  [1](http://noamz.org/DC2019-roundrobin/GD-vs-KI-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/GD-vs-KI-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/GD-vs-KI-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/GD-vs-KI-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/GD-vs-KI-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/GD-vs-KI-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/GD-vs-KI-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/GD-vs-KI-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/GD-vs-KI-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/GD-vs-KI-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/GD-vs-KI-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/GD-vs-KI-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/GD-vs-KI-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/GD-vs-KI-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/GD-vs-KI-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/GD-vs-KI-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/GD-vs-KI-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/GD-vs-KI-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/GD-vs-KI-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/GD-vs-KI-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/GD-vs-KI-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/GD-vs-KI-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/GD-vs-KI-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/GD-vs-KI-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/GD-vs-KI-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/GD-vs-KI-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/GD-vs-KI-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/GD-vs-KI-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/GD-vs-KI-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/GD-vs-KI-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/GD-vs-KI-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/GD-vs-KI-32.svg)

* **KI vs RD**
  * H = KI, V = RD:
  [1](http://noamz.org/DC2019-roundrobin/KI-vs-RD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/KI-vs-RD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/KI-vs-RD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/KI-vs-RD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/KI-vs-RD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/KI-vs-RD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/KI-vs-RD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/KI-vs-RD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/KI-vs-RD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/KI-vs-RD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/KI-vs-RD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/KI-vs-RD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/KI-vs-RD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/KI-vs-RD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/KI-vs-RD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/KI-vs-RD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/KI-vs-RD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/KI-vs-RD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/KI-vs-RD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/KI-vs-RD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/KI-vs-RD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/KI-vs-RD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/KI-vs-RD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/KI-vs-RD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/KI-vs-RD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/KI-vs-RD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/KI-vs-RD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/KI-vs-RD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/KI-vs-RD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/KI-vs-RD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/KI-vs-RD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/KI-vs-RD-32.svg)
  * H = RD, V = KI:
  [1](http://noamz.org/DC2019-roundrobin/RD-vs-KI-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/RD-vs-KI-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/RD-vs-KI-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/RD-vs-KI-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/RD-vs-KI-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/RD-vs-KI-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/RD-vs-KI-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/RD-vs-KI-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/RD-vs-KI-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/RD-vs-KI-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/RD-vs-KI-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/RD-vs-KI-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/RD-vs-KI-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/RD-vs-KI-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/RD-vs-KI-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/RD-vs-KI-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/RD-vs-KI-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/RD-vs-KI-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/RD-vs-KI-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/RD-vs-KI-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/RD-vs-KI-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/RD-vs-KI-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/RD-vs-KI-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/RD-vs-KI-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/RD-vs-KI-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/RD-vs-KI-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/RD-vs-KI-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/RD-vs-KI-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/RD-vs-KI-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/RD-vs-KI-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/RD-vs-KI-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/RD-vs-KI-32.svg)

* **RD vs GD**
  * H = RD, V = GD:
  [1](http://noamz.org/DC2019-roundrobin/RD-vs-GD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/RD-vs-GD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/RD-vs-GD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/RD-vs-GD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/RD-vs-GD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/RD-vs-GD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/RD-vs-GD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/RD-vs-GD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/RD-vs-GD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/RD-vs-GD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/RD-vs-GD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/RD-vs-GD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/RD-vs-GD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/RD-vs-GD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/RD-vs-GD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/RD-vs-GD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/RD-vs-GD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/RD-vs-GD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/RD-vs-GD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/RD-vs-GD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/RD-vs-GD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/RD-vs-GD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/RD-vs-GD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/RD-vs-GD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/RD-vs-GD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/RD-vs-GD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/RD-vs-GD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/RD-vs-GD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/RD-vs-GD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/RD-vs-GD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/RD-vs-GD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/RD-vs-GD-32.svg)
  * H = GD, V = RD:
  [1](http://noamz.org/DC2019-roundrobin/GD-vs-RD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/GD-vs-RD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/GD-vs-RD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/GD-vs-RD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/GD-vs-RD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/GD-vs-RD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/GD-vs-RD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/GD-vs-RD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/GD-vs-RD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/GD-vs-RD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/GD-vs-RD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/GD-vs-RD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/GD-vs-RD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/GD-vs-RD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/GD-vs-RD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/GD-vs-RD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/GD-vs-RD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/GD-vs-RD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/GD-vs-RD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/GD-vs-RD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/GD-vs-RD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/GD-vs-RD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/GD-vs-RD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/GD-vs-RD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/GD-vs-RD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/GD-vs-RD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/GD-vs-RD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/GD-vs-RD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/GD-vs-RD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/GD-vs-RD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/GD-vs-RD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/GD-vs-RD-32.svg)

* **BT vs AG**
  * H = BT, V = AG:
  [1](http://noamz.org/DC2019-roundrobin/BT-vs-AG-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/BT-vs-AG-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/BT-vs-AG-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/BT-vs-AG-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/BT-vs-AG-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/BT-vs-AG-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/BT-vs-AG-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/BT-vs-AG-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/BT-vs-AG-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/BT-vs-AG-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/BT-vs-AG-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/BT-vs-AG-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/BT-vs-AG-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/BT-vs-AG-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/BT-vs-AG-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/BT-vs-AG-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/BT-vs-AG-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/BT-vs-AG-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/BT-vs-AG-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/BT-vs-AG-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/BT-vs-AG-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/BT-vs-AG-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/BT-vs-AG-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/BT-vs-AG-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/BT-vs-AG-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/BT-vs-AG-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/BT-vs-AG-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/BT-vs-AG-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/BT-vs-AG-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/BT-vs-AG-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/BT-vs-AG-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/BT-vs-AG-32.svg)
  * H = AG, V = BT:
  [1](http://noamz.org/DC2019-roundrobin/AG-vs-BT-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AG-vs-BT-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AG-vs-BT-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AG-vs-BT-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AG-vs-BT-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AG-vs-BT-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AG-vs-BT-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AG-vs-BT-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AG-vs-BT-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AG-vs-BT-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AG-vs-BT-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AG-vs-BT-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AG-vs-BT-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AG-vs-BT-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AG-vs-BT-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AG-vs-BT-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AG-vs-BT-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AG-vs-BT-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AG-vs-BT-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AG-vs-BT-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AG-vs-BT-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AG-vs-BT-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AG-vs-BT-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AG-vs-BT-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AG-vs-BT-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AG-vs-BT-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AG-vs-BT-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AG-vs-BT-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AG-vs-BT-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AG-vs-BT-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AG-vs-BT-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AG-vs-BT-32.svg)

* **BT vs GD**
  * H = BT, V = GD:
  [1](http://noamz.org/DC2019-roundrobin/BT-vs-GD-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/BT-vs-GD-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/BT-vs-GD-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/BT-vs-GD-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/BT-vs-GD-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/BT-vs-GD-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/BT-vs-GD-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/BT-vs-GD-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/BT-vs-GD-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/BT-vs-GD-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/BT-vs-GD-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/BT-vs-GD-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/BT-vs-GD-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/BT-vs-GD-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/BT-vs-GD-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/BT-vs-GD-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/BT-vs-GD-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/BT-vs-GD-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/BT-vs-GD-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/BT-vs-GD-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/BT-vs-GD-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/BT-vs-GD-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/BT-vs-GD-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/BT-vs-GD-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/BT-vs-GD-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/BT-vs-GD-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/BT-vs-GD-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/BT-vs-GD-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/BT-vs-GD-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/BT-vs-GD-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/BT-vs-GD-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/BT-vs-GD-32.svg)
  * H = GD, V = BT:
  [1](http://noamz.org/DC2019-roundrobin/GD-vs-BT-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/GD-vs-BT-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/GD-vs-BT-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/GD-vs-BT-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/GD-vs-BT-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/GD-vs-BT-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/GD-vs-BT-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/GD-vs-BT-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/GD-vs-BT-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/GD-vs-BT-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/GD-vs-BT-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/GD-vs-BT-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/GD-vs-BT-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/GD-vs-BT-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/GD-vs-BT-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/GD-vs-BT-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/GD-vs-BT-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/GD-vs-BT-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/GD-vs-BT-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/GD-vs-BT-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/GD-vs-BT-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/GD-vs-BT-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/GD-vs-BT-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/GD-vs-BT-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/GD-vs-BT-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/GD-vs-BT-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/GD-vs-BT-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/GD-vs-BT-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/GD-vs-BT-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/GD-vs-BT-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/GD-vs-BT-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/GD-vs-BT-32.svg)

* **AG vs KI**
  * H = AG, V = KI:
  [1](http://noamz.org/DC2019-roundrobin/AG-vs-KI-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/AG-vs-KI-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/AG-vs-KI-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/AG-vs-KI-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/AG-vs-KI-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/AG-vs-KI-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/AG-vs-KI-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/AG-vs-KI-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/AG-vs-KI-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/AG-vs-KI-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/AG-vs-KI-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/AG-vs-KI-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/AG-vs-KI-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/AG-vs-KI-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/AG-vs-KI-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/AG-vs-KI-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/AG-vs-KI-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/AG-vs-KI-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/AG-vs-KI-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/AG-vs-KI-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/AG-vs-KI-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/AG-vs-KI-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/AG-vs-KI-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/AG-vs-KI-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/AG-vs-KI-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/AG-vs-KI-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/AG-vs-KI-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/AG-vs-KI-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/AG-vs-KI-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/AG-vs-KI-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/AG-vs-KI-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/AG-vs-KI-32.svg)
  * H = KI, V = AG:
  [1](http://noamz.org/DC2019-roundrobin/KI-vs-AG-1.svg)
  [2](http://noamz.org/DC2019-roundrobin/KI-vs-AG-2.svg)
  [3](http://noamz.org/DC2019-roundrobin/KI-vs-AG-3.svg)
  [4](http://noamz.org/DC2019-roundrobin/KI-vs-AG-4.svg)
  [5](http://noamz.org/DC2019-roundrobin/KI-vs-AG-5.svg)
  [6](http://noamz.org/DC2019-roundrobin/KI-vs-AG-6.svg)
  [7](http://noamz.org/DC2019-roundrobin/KI-vs-AG-7.svg)
  [8](http://noamz.org/DC2019-roundrobin/KI-vs-AG-8.svg)
  [9](http://noamz.org/DC2019-roundrobin/KI-vs-AG-9.svg)
  [10](http://noamz.org/DC2019-roundrobin/KI-vs-AG-10.svg)
  [11](http://noamz.org/DC2019-roundrobin/KI-vs-AG-11.svg)
  [12](http://noamz.org/DC2019-roundrobin/KI-vs-AG-12.svg)
  [13](http://noamz.org/DC2019-roundrobin/KI-vs-AG-13.svg)
  [14](http://noamz.org/DC2019-roundrobin/KI-vs-AG-14.svg)
  [15](http://noamz.org/DC2019-roundrobin/KI-vs-AG-15.svg)
  [16](http://noamz.org/DC2019-roundrobin/KI-vs-AG-16.svg)
  [17](http://noamz.org/DC2019-roundrobin/KI-vs-AG-17.svg)
  [18](http://noamz.org/DC2019-roundrobin/KI-vs-AG-18.svg)
  [19](http://noamz.org/DC2019-roundrobin/KI-vs-AG-19.svg)
  [20](http://noamz.org/DC2019-roundrobin/KI-vs-AG-20.svg)
  [21](http://noamz.org/DC2019-roundrobin/KI-vs-AG-21.svg)
  [22](http://noamz.org/DC2019-roundrobin/KI-vs-AG-22.svg)
  [23](http://noamz.org/DC2019-roundrobin/KI-vs-AG-23.svg)
  [24](http://noamz.org/DC2019-roundrobin/KI-vs-AG-24.svg)
  [25](http://noamz.org/DC2019-roundrobin/KI-vs-AG-25.svg)
  [26](http://noamz.org/DC2019-roundrobin/KI-vs-AG-26.svg)
  [27](http://noamz.org/DC2019-roundrobin/KI-vs-AG-27.svg)
  [28](http://noamz.org/DC2019-roundrobin/KI-vs-AG-28.svg)
  [29](http://noamz.org/DC2019-roundrobin/KI-vs-AG-29.svg)
  [30](http://noamz.org/DC2019-roundrobin/KI-vs-AG-30.svg)
  [31](http://noamz.org/DC2019-roundrobin/KI-vs-AG-31.svg)
  [32](http://noamz.org/DC2019-roundrobin/KI-vs-AG-32.svg)

Final ranking from the round-robin tournament:
```
AG = 32
BT = 32
AD = 32
GD = 16
KI = 8
RD = 0
```

After the round-robin tournament, AG, BT, and AD were all tied for first place.
However, on further inspection, it turned out that AD was Alpha Dom, and so was eliminated from the competition for not meeting the eligibility requirements.

The championship match was then played between AG and BT.

Results of the championship match:
```
Playing 32 games with AG = H and BT = V
Player H loses!
Player V loses!
Player V loses!
Player V loses!
Player V loses!
Player H loses!
Player V loses!
Player V loses!
Player V loses!
Player H loses!
Player H loses!
Player H loses!
Player V loses!
Player V loses!
Player H loses!
Player V loses!
Player V loses!
Player V loses!
Player V loses!
Player V loses!
Player V loses!
Player H loses!
Player V loses!
Player H loses!
Player V loses!
Player V loses!
Player H loses!
Player V loses!
Player V loses!
Player V loses!
Player H loses!
Player V loses!
Playing 32 games with AG = V and BT = H
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player V loses!
Player V loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player H loses!
Player V loses!
Player H loses!
Player H loses!
Player H loses!
Player V loses!
Player H loses!
Player V loses!
Player V loses!
Player V loses!
Player V loses!
Player V loses!
Player H loses!
Player H loses!
Player H loses!
```
Complete record of championship match:

* **AG vs BT**
  * H = AG, V = BT:
  [1](http://noamz.org/DC2019-championship/AG-vs-BT-1.svg)
  [2](http://noamz.org/DC2019-championship/AG-vs-BT-2.svg)
  [3](http://noamz.org/DC2019-championship/AG-vs-BT-3.svg)
  [4](http://noamz.org/DC2019-championship/AG-vs-BT-4.svg)
  [5](http://noamz.org/DC2019-championship/AG-vs-BT-5.svg)
  [6](http://noamz.org/DC2019-championship/AG-vs-BT-6.svg)
  [7](http://noamz.org/DC2019-championship/AG-vs-BT-7.svg)
  [8](http://noamz.org/DC2019-championship/AG-vs-BT-8.svg)
  [9](http://noamz.org/DC2019-championship/AG-vs-BT-9.svg)
  [10](http://noamz.org/DC2019-championship/AG-vs-BT-10.svg)
  [11](http://noamz.org/DC2019-championship/AG-vs-BT-11.svg)
  [12](http://noamz.org/DC2019-championship/AG-vs-BT-12.svg)
  [13](http://noamz.org/DC2019-championship/AG-vs-BT-13.svg)
  [14](http://noamz.org/DC2019-championship/AG-vs-BT-14.svg)
  [15](http://noamz.org/DC2019-championship/AG-vs-BT-15.svg)
  [16](http://noamz.org/DC2019-championship/AG-vs-BT-16.svg)
  [17](http://noamz.org/DC2019-championship/AG-vs-BT-17.svg)
  [18](http://noamz.org/DC2019-championship/AG-vs-BT-18.svg)
  [19](http://noamz.org/DC2019-championship/AG-vs-BT-19.svg)
  [20](http://noamz.org/DC2019-championship/AG-vs-BT-20.svg)
  [21](http://noamz.org/DC2019-championship/AG-vs-BT-21.svg)
  [22](http://noamz.org/DC2019-championship/AG-vs-BT-22.svg)
  [23](http://noamz.org/DC2019-championship/AG-vs-BT-23.svg)
  [24](http://noamz.org/DC2019-championship/AG-vs-BT-24.svg)
  [25](http://noamz.org/DC2019-championship/AG-vs-BT-25.svg)
  [26](http://noamz.org/DC2019-championship/AG-vs-BT-26.svg)
  [27](http://noamz.org/DC2019-championship/AG-vs-BT-27.svg)
  [28](http://noamz.org/DC2019-championship/AG-vs-BT-28.svg)
  [29](http://noamz.org/DC2019-championship/AG-vs-BT-29.svg)
  [30](http://noamz.org/DC2019-championship/AG-vs-BT-30.svg)
  [31](http://noamz.org/DC2019-championship/AG-vs-BT-31.svg)
  [32](http://noamz.org/DC2019-championship/AG-vs-BT-32.svg)
  * H = BT, V = AG:
  [1](http://noamz.org/DC2019-championship/BT-vs-AG-1.svg)
  [2](http://noamz.org/DC2019-championship/BT-vs-AG-2.svg)
  [3](http://noamz.org/DC2019-championship/BT-vs-AG-3.svg)
  [4](http://noamz.org/DC2019-championship/BT-vs-AG-4.svg)
  [5](http://noamz.org/DC2019-championship/BT-vs-AG-5.svg)
  [6](http://noamz.org/DC2019-championship/BT-vs-AG-6.svg)
  [7](http://noamz.org/DC2019-championship/BT-vs-AG-7.svg)
  [8](http://noamz.org/DC2019-championship/BT-vs-AG-8.svg)
  [9](http://noamz.org/DC2019-championship/BT-vs-AG-9.svg)
  [10](http://noamz.org/DC2019-championship/BT-vs-AG-10.svg)
  [11](http://noamz.org/DC2019-championship/BT-vs-AG-11.svg)
  [12](http://noamz.org/DC2019-championship/BT-vs-AG-12.svg)
  [13](http://noamz.org/DC2019-championship/BT-vs-AG-13.svg)
  [14](http://noamz.org/DC2019-championship/BT-vs-AG-14.svg)
  [15](http://noamz.org/DC2019-championship/BT-vs-AG-15.svg)
  [16](http://noamz.org/DC2019-championship/BT-vs-AG-16.svg)
  [17](http://noamz.org/DC2019-championship/BT-vs-AG-17.svg)
  [18](http://noamz.org/DC2019-championship/BT-vs-AG-18.svg)
  [19](http://noamz.org/DC2019-championship/BT-vs-AG-19.svg)
  [20](http://noamz.org/DC2019-championship/BT-vs-AG-20.svg)
  [21](http://noamz.org/DC2019-championship/BT-vs-AG-21.svg)
  [22](http://noamz.org/DC2019-championship/BT-vs-AG-22.svg)
  [23](http://noamz.org/DC2019-championship/BT-vs-AG-23.svg)
  [24](http://noamz.org/DC2019-championship/BT-vs-AG-24.svg)
  [25](http://noamz.org/DC2019-championship/BT-vs-AG-25.svg)
  [26](http://noamz.org/DC2019-championship/BT-vs-AG-26.svg)
  [27](http://noamz.org/DC2019-championship/BT-vs-AG-27.svg)
  [28](http://noamz.org/DC2019-championship/BT-vs-AG-28.svg)
  [29](http://noamz.org/DC2019-championship/BT-vs-AG-29.svg)
  [30](http://noamz.org/DC2019-championship/BT-vs-AG-30.svg)
  [31](http://noamz.org/DC2019-championship/BT-vs-AG-31.svg)
  [32](http://noamz.org/DC2019-championship/BT-vs-AG-32.svg)

Final score from the championship match:
```
Final score: 45 (AG) vs 19 (BT)
AG wins!
```

Congratulations to AG for winning the 2019 Domineering Challenge!
And kudos to all the other contestants for taking on the Challenge!
