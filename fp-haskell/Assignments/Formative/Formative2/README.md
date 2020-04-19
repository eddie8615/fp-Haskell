# Formative assignment 2

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

* Go into the "Assignments/Formative/Formative2" directory and *copy* the given `Formative2Template.hs` to a *new* file `Formative2.hs`.
  Work on that file to produce your solution and then submit it to Canvas.

  It is important to copy the file as explained, since any future `git pull` may overwrite the template. Hence don't work directly on the template.

* If you work inside another directory besides the git repository, you will also need to copy the file `Types.hs`, which includes type definitions that are imported by the template file.
  Any data or types that you define **yourself** should be in `Formative2.hs`, but the ones defined in the assignment are and should stay in `Types.hs`.
  You should **not** modify the `Types.hs` file.

* Be aware that:

  * Your solutions must work with GHC 8.6.5. To use GHC 8.6.5 on a lab machine, see [HardwareAndSoftware.md](../../Resources/HardwareAndSoftware.md).

  * If you wish to import modules, then you may only import libraries from [the standard library](http://hackage.haskell.org/package/base). Additionally, all modules you import must be "Safe" on Hackage.

## Questions

The first three questions build on the following data type of trees:
```haskell
data Tree a b = Leaf a | Node b [Tree a b]
     deriving (Show,Eq)
```
This is a type of tree where every leaf contains a value of type `a`, and where every node contains a value of type `b` together with an ordered list of children.

For example, the tree
```
              ________1_______
             |        |       |
texample =  _2_     __3__     4
           |   |   |  |  |
           a   b   c  d  e
```
(which contains integers at the nodes and characters at the leaves)
can be represented by the following value of type `Tree Char Integer`:
```haskell
texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]
```
Here is another example of a very skinny tree (perhaps it is more of a bamboo stalk?)
```
          1
          |
          2
          |
tskinny = 3
          |
          4
          |
          a
```
which can be represented as a value of the same type:
```haskell
tskinny = Node 1 [Node 2 [Node 3 [Node 4 [Leaf 'a']]]]
```

1. Write a function
   ```haskell
   canopy :: Tree a b -> [a]
   canopy = undefined
   ```
   that computes the *canopy* of a tree (also called its "fringe"), that is, the list of leaves of the tree as they appear in an in-order traversal.

   Examples:
   ```hs
   > canopy texample
   "abcde"
   > canopy tskinny
   "a"
   ```

2. Write a function
   ```haskell
   treePreOrder :: Tree a b -> [Either a b]
   treePreOrder = undefined
   ```
   that computes the left-to-right pre-order traversal of a tree.
   (Note that here we need to use an [Either type](../../../LectureNotes/data.md#either) since the traversal can potentially contain different types of values coming from the leaves and from the nodes.)

   Examples:
   ```hs
   > treePreOrder texample
   [Right 1,Right 2,Left 'a',Left 'b',Right 3,Left 'c',Left 'd',Left 'e',Right 4]
   > treePreOrder tskinny
   [Right 1,Right 2,Right 3,Right 4,Left 'a']
   ```

3. An *operation tree* is a tree where each node is labelled by an operation (for us, either addition or multiplication):
   ```haskell
   data Op = Add | Mul  deriving (Show,Eq)
   type OpTree a = Tree a Op
   ```
   For example, the operation tree
   ```
               ________+_______
              |        |       |
   texpr =   _*_     __*__     *
            |   |   |  |  |
            1   2   3  4  5
   ```
   can be represented by the following value of type `OpTree Integer`:
   ```haskell
   texpr = Node Add [Node Mul [Leaf 1, Leaf 2], Node Mul [Leaf 3, Leaf 4, Leaf 5], Node Mul []]
   ```

   Define a function
   ```haskell
   eval :: Num a => OpTree a -> a
   eval = undefined
   ```
   which takes an operation tree and evaluates it to a number, interpreting `Add` nodes by n-ary addition and `Mul` nodes by n-ary multiplication.
   Note that by convention in mathematics, 0-ary addition (i.e., the sum of the empty list of values) is defined to be 0, while 0-ary multiplication (i.e., the product of the empty list of values) is defined to be 1.
   
   Examples:
   ```hs
   > eval texpr
   63
   > eval (Node Mul [Node Add [Leaf 1, Leaf 2], Node Add [Leaf 3, Leaf 4]])
   21
   ```

4. As you may remember from [the notes](../../../LectureNotes/data.md#bstsort), a form of [Quicksort](https://en.wikipedia.org/wiki/Quicksort) is easy to write in Haskell:
   ```hs
   qsort :: Ord a => [a] -> [a]
   qsort [] = []
   qsort (x:xs) = qsort [l | l <- xs, l < x]
               ++ [x]
               ++ qsort [r | r <- xs, r >= x]
   ```
   However, this implementation &mdash; where in the recursive case we always pick the first element of the list as the "pivot" &mdash; has the [serious defect](https://en.wikipedia.org/wiki/Quicksort#Choice_of_pivot) that it leads to O(n²) running time on lists that are nearly sorted (or reverse-sorted).
   A better way of implementing Quicksort, with expected O(n log n) running time, is to instead pick a random element of the list as a pivot.

   In order to encapsulate the action of picking a pivot, let us introduce a class of "picking monads":
   ```haskell
   class Monad m => PickingMonad m where
      pick :: Int -> Int -> m Int
   ```
   A picking monad includes all of the operations of an ordinary monad (`return` and `>>=`), but also an operation `pick lo hi`, with the intended interpretation that it should pick a number between `lo` and `hi` (i.e. `lo <= pick lo hi <= hi`) in some way (say by calling a random number generator, or by returning their mean).
   
   **Problem:** Implement a new version of Quicksort
   ```haskell
   qsort :: (Ord a, PickingMonad m) => [a] -> m [a]
   qsort = undefined
   ```
   which makes appropriate calls to `pick` in order to choose the pivot.
   Your new version should:
   1. have expected running time O(n log n) given an implementation of `pick lo hi` that always returns a uniformly random number between `lo` and `hi`
   2. behave equivalently to the original version given an implementation of `pick lo hi` that always returns `lo`

   For testing purposes, you might find it convenient to consider the following instances of `PickingMonad`, which implement `pick lo hi` by calling the system random number generator, or by always returning `lo`, respectively:
   ```haskell
   instance PickingMonad IO where
     pick lo hi = getStdRandom (randomR (lo, hi))

   instance PickingMonad Identity where
     pick lo hi = Identity lo
   ```

   Examples:
   ```hs
   > qsort [10,9..1] :: IO [Integer]
   [1,2,3,4,5,6,7,8,9,10]
   > qsort [10,9..1] :: Identity [Integer]
   Identity [1,2,3,4,5,6,7,8,9,10]
   ```

   
