# Lab 1. Get acquainted with the Haskell environment provided in the lab

This is not assessed. But it is rather important for everything that
follows in the module.

* You will get acquainted with the ghc
environment provided in the lab and write simple programs to start getting used to
Haskell.

* You can develop your assignments in your own machine, but they must run in the lab machines, as we perform the marking there.
  In particular, follow the instructions at [Hardware and Software](../../Resources/HardwareAndSoftware.md) to make sure you have the correct working environment.

# Login to a machine in the lab

* Get the learning git repository, by opening a terminal and running

  ```shell
  git clone https://git.cs.bham.ac.uk/zeilbern/fp-learning-2019-2020.git
  ```

* Then set up the environment for Haskell by running

  ```shell
  module load func-prog
  ```

* Then [run ghci](http://learnyouahaskell.com/starting-out#ready-set-go) and do the examples in that page. Do `:quit` when you have run enough examples.

* Then see [how to load source files](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html).

* Read the notes [Introduction to functional programming](../LectureNotes/Introduction.md).
  Then convert those markdown notes into a Haskell file:

  ```shell
  $ cd fp-learning-2019-2020/LectureNotes
  $ cat Introduction.md | runhaskell ../Resources/mdtohs.hs > Introduction.hs
  ```
  And try running some of the examples:
  ```shell
  $ ghci Introduction.hs
  GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
  [1 of 1] Compiling Main             ( Introduction.hs, interpreted )
  Ok, modules loaded: Main.
  *Main> merge [5..10] [1..20]
  [1,2,3,4,5,5,6,6,7,7,8,8,9,9,10,10,11,12,13,14,15,16,17,18,19,20]
  *Main>
  ```

* Copy the function `merge` from the file "Introduction.hs" to a new file, and define a function [`msort`](https://en.wikipedia.org/wiki/Merge_sort).

* Try to run it on some examples to test it
  ```
  > :load <your file>
  > let xs = [1..1000]
  > length xs == length(msort xs)
  > msort (reverse xs) == xs
  > msort xs == xs
  ```

  Write a function `isSorted :: Ord a => [a] -> Bool` that checks whether a list is sorted, and then test:
  ```
  > isSorted xs
  > not(isSorted(reverse xs))
  > isSorted (msort (reverse xs))
  ```

Challenging exercise: Write a function `isPermutation :: Eq a => [a] -> [a] -> Bool` to check whether two lists are permutations of each other. Check that sorting gives permutations in suitable examples.


* You may wish to explore our textbook, by trying to run some of the [code examples](../Resources/Book/code). You don't need to understand how they work. Just try to run some of them.

* When you finish, report your work as [Task 1](https://canvas.bham.ac.uk/courses/38415/assignments/196798) on Canvas.

* Then read [this page](../README.md) carefully to familiarize yourself with the module requirements and facilities.

* Finally you can get started with [Task 2](https://canvas.bham.ac.uk/courses/38415/assignments/196809). (Read the book.)
