## Lecture Log

The log for each lecture will be updated before and after the lecture takes place, sometimes more than once.

* We have some [lecture notes](../LectureNotes/README.md) to complement the lectures and the book.

#### Lecture style

We provide the official textbook's slides, and although we may use them occasionally during the lectures, it is more effective to use our own material and the board for the lecture and this is what we'll do.

---

## Week 1

#### Monday 30/09/2019 (Introduction)

###### Learning objectives

* The historical context of functional programming in general and Haskell in particular.

* Some initial examples of Haskell code with explanation of the language.

###### Lecture

* Introductions (please read [Important General Information](../README.md))

* We discussed a Haskell implementation of the [Game of Life](../LectureNotes/Life.md).
  (We made it through "take #1" and a bit of "take #2" before running out of time.)

###### Lab

* [Get acquainted with the Haskell environment provided in the lab](../Assignments/Labs/Lab1.md)

Here are some book slides for reinforcement:

* [Introduction](Book/slides/PDF/ch1.pdf)

* [First steps](Book/slides/PDF/ch2.pdf)

* [Types and classes](Book/slides/PDF/ch3.pdf)

* [Defining functions](Book/slides/PDF/ch4.pdf)

* [List comprehensions](Book/slides/PDF/ch5.pdf)

And the corresponding book chapters.

#### Wednesday 02/10/2019 (User defined data types, part 1)

###### Learning objectives

* Basic syntax for data types in Haskell

* Defining functions by pattern-matching

* Type synonyms vs type isomorphisms

* Recursively defined data types (lists)

* Many examples of interest.

###### Lecture

* We finished the [Game of Life](../LectureNotes/Life.md) example, and also mentioned the [Hoogle](https://hoogle.haskell.org/) search engine which is useful for learning the Haskell API.

* We explained the syntax for type synonyms (`type String = [Char]`) and a bit of their motivation.

* We explained the syntax for data type definitions, starting from the example of `Bool`.

* We discussed the (sometimes subtle!) semantics of pattern-matching.

* We discussed some type isomorphisms (`Bool` ≈ `BW` ≈ `Bit`) and the difference between type isomorphisms and type synonyms.

* A bit about the `deriving` mechanism in Haskell.

* Important examples: `Maybe a`, `Either a b`, and `And a b` ≈ `(a,b)`

* We started discussing lists as a recursively defined data type, and the definitions of list concatenation and reversal.

Here are some book slides for reinforcement:

* [Recursive functions](Book/slides/PDF/ch6.pdf)

* [Higher-order functions](Book/slides/PDF/ch7.pdf)

* [Declaring types and type classes](Book/slides/PDF/ch8.pdf)

And the corresponding book chapters.

---

## Week 2

#### Monday 07/10/2019 (User defined data types, part 2)

###### Learning objectives

* Getting more familiar with lists

* Defining functions using accumulators

* Higher-order functions

* Defining and programming with binary trees

###### Lecture

* We reviewed recursive definitions of list concatenation and list reversal on our isomorphic copy of the list type

* We discussed an efficiency bug in the definition of reversal, as well as a fix that required introducing a new function `revapp`

* We talked about the more general idea of "accumulators" with the example of computing the Fibonacci numbers

* We looked at the definitions of a few important higher-order functions on lists (`map`, `filter`, `foldr`, `foldl`)

* We introduced the data type of binary trees and wrote a few basic functions like `mirror`, `size`, and `height`

---

#### Wednesday 09/10/2019 (User defined data types, part 3)

###### Learning objectives

* More programming with (and reasoning) about the data type of binary trees

###### Lecture

* We spent a good deal of time going over the Formative 1 assignment and different possible [solutions](../Assignments/Formative/Formative1/Formative1Solutions.hs)

* We quickly went over the questions for the [Assessed 1](../Assignments/Assessed/Assessed1/README.md) assignment

* We picked up the conversation on binary trees from Monday's lecture &mdash; beginning with a small digression on different possible variations of the data type of binary trees (with different types of labels at the nodes and/or the leaves, cf. the discussion at the end of Section 8.4 of the Book)

* We defined functions for accessing subtrees of a tree by their "address" (= a list of directions), and for listing all of the valid addresses of a tree

* We talked a little bit about the principle of induction for proving properties of trees

* We talked about computing the in-order, pre-order, and breadth-first traversals of a tree

* We started to talk about the problem of "inverting" a traversal

---

## Week 3

---

#### Monday 14/10/2019 (User defined data types, part 4)

###### Learning objectives

* Continuation of the previous lecture

###### Lecture

* We continued discussing the problem of "inverting" a traversal (and making more precise what exactly this means!)

* We went over the definition of the function `inOrderTree :: [a] -> [BT a]` computing the list of all binary trees with a given in-order traversal, and its precise relationship to the function `treeInOrder :: BT a -> [a]`.

* We introduced binary search trees and discussed the implementations of some operations

* We talked a bit about representing other kinds of trees: rose trees, game trees, and expression trees

#### Wednesday 16/10/2019 (User defined data types, part 5 + type classes + monads, part 1)

###### Learning objectives

* Continuation of the previous lecture

* Getting more familiar with type classes

* Introduction to monads

###### Lecture

* We reviewed and completed the discussion of [game trees](../LectureNotes/data.md#gametrees) from Monday, looking at the example of "Nim"

* We spent some time going over the basics of [type classes and instances](../LectureNotes/typeclasses.md) and some examples from the Haskell prelude

* We got a first taste of [monads](../LectureNotes/monads.md)

---

## Week 4

---

#### Monday 21/10/2019 (Monads, part 2)

###### Learning objectives

* Understanding the definition of Haskell monads (in "old style")

* Basic examples of monads

* Monadic programming and do notation

###### Lecture

* We motivated [monads](../LectureNotes/monads.md) as a way of dealing with side-effects in Haskell, and as a kind of generalised "pipeline" for passing values from one function to another

* We discussed the simple but important examples of the Maybe monad and the List monad, before considering the general definition of monads

* We reviewed the definition of the Monad typeclass in "old style" (without factoring via `Functor` and `Applicative`), and the operations `return` and `(>>=)` (pronounced "bind")

* We introduced Haskell's "do notation" for programming with monads

#### Wednesday 23/10/2019 (Monads, part 3)

###### Learning objectives

* Continuation of the previous lecture

###### Lecture

* We reviewed the Maybe monad, the List monad, and do notation

* Using the basic example of addition, and later with the more interesting example of computing Fibonacci numbers, we saw how one program can be given multiple interpretations in different monads

* We introduced some more monads: Identity, Writer, Counter

* In the last half-hour of class, we went over the [solutions to Assessed 1](../Assignments/Assessed/Assessed1/Assessed1Solutions.hs)

---

## Week 5

---

#### Monday 28/10/2019 (Monads, part 4)

###### Learning objectives

* Deeper into monads

###### Lecture

* We discussed the "new style" definition of monads in Haskell, where the `Monad` class is factored via `Functor` and `Applicative`

* In particular, we talked about the operation `fmap` of the `Functor` class, and the operations `pure` and `<*>` of the `Applicative` class

* We saw how to convert between "old style" and "new style" monads

#### Wednesday 30/10/2019 (Monads, part 5)

###### Learning objectives

* Continuation of the previous lecture, and understanding the State monad

###### Lecture

* We introduced the State monad, and saw how to use it for another implementation of `fib`

* We looked in detail at the definition of return and bind for the State monad

* In the second half of class, we finished going over the solutions to Assessed 1, moved onto discuss the [solutions to Formative 2](../Assignments/Formative/Formative2/Formative2Solutions.hs), and finally spent some time introducing [Assessed 2](../Assignments/Assessed/Assessed2/README.md)

* In particular, as background to Assessed 2, we emphasised the idea of the List monad as a way of representing non-deterministic computation, and then looked at the monad `Dist` as a refinement of the List monad for representing probabilistic computation.

---


## Week 6

---

#### Monday 04/11/2019

###### Learning objectives

* The differences between "pre-rigorous", "rigorous", and "post-rigorous" programming

* Understanding how to give a rigorous specification or partial specification of a function

###### Lecture

* We followed the notes on [rigorous programming](../LectureNotes/rigour.md)

#### Wednesday 06/11/2019

###### Learning objectives

* Continuation of the previous lecture

* Basics of continuation-passing style

###### Lecture

* We continued the notes on [rigorous programming](../LectureNotes/rigour.md)

* We began the notes on [continuation-passing style](../LectureNotes/continuations.md)

---

## Week 7

---

#### Monday 11/11/2019

###### Learning objectives

* Rigorous specification in Agda

###### Lecture

* We followed the notes on [specifications and correctness proofs of some of the programs we've seen in lectures](../LectureNotes/Agda-in-a-Hurry.agda).

#### Wednesday 13/11/2019

###### Learning objectives

* Monad laws

* Continuing with continuations

###### Lecture

* We discussed the [monad laws](../LectureNotes/monads.lagda).

* We discussed the solutions to the Assessed 2 assignment

* We continued with the notes on [continuation-passing style](../LectureNotes/continuations.md), making it up through basic programming with success and failure continuations, but not yet to the example of the SAT solver.

---

## Week 8

---

#### Monday 18/11/2019


###### Learning objectives

* Programming with success and failure continuations

* Backtracking

###### Lecture

* After recalling the technique of [programming with success and failure continuations](../LectureNotes/continuations.md#succfail), we studied how to use this technique to program a [backtracking SAT solver](../LectureNotes/satsolver.md).

#### Wednesday 20/11/2019

###### Learning objectives

* Writing a more involved Haskell program

* Game-playing with minimax search

* An application of laziness

###### Lecture

* We followed the notes on [unbeatable tic-tac-toe](../LectureNotes/tictactoe.md).

---

## Week 9

---

#### Monday 25/11/2019

###### Learning objectives

* Understanding evaluation in functional languages

* Call-by-value, call-by-name, and call-by-need (lazy) evaluation

###### Lecture

* We followed the notes on [laziness and infinite data structures](../LectureNotes/lazy.md).

#### Wednesday 27/11/2019

###### Learning objectives

* Continuation of the previous lecture

* Combining laziness with infinite (cyclic) data structures

###### Lecture

* We continued the notes on [laziness and infinite data structures](../LectureNotes/lazy.md).

---

## Week 10

---

#### Monday 02/12/2019

###### Learning objectives

* Writing a more involved Haskell program
* Writing an interpreter

###### Lecture

* We began the notes on [Implementing a small imperative language](../LectureNotes/interpreter/README.md).

#### Wednesday 04/12/2019

* Continuation of the previous lecture.

###### Lecture

* We continued the notes on [Implementing a small imperative language](../LectureNotes/interpreter/README.md), and also spoke a bit about [monadic parsing](Book/code/Parsing.hs).

---

## Week 11

---

#### Monday 09/12/2019

This lecture slot has been "donated" &mdash; please use it for a good cause!
(Such as working on [Assessed 3](../Assignments/Assessed/Assessed3/README.md), or the AFP Domineering Challenge.).

#### Wednesday 11/12/2019

Sample exam questions were handed out, to be completed in the first 50 minutes. (They are available [on Canvas](https://canvas.bham.ac.uk/courses/38415/quizzes/75521).)
Then we took a group photo (actually several, while trying out different functional programming terms such as "monads" and "continuation-passing style").
Finally, we went over the results of the round-robin tournament from the [Domineering Challenge](../Assignments/Assessed/Assessed3/README.md#results), and held the championship match live!

Thank you all for your hard work over the semester!
