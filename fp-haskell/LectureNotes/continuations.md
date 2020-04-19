# Contents

* [Continuation-passing style](#cps)
  * [Background](#background)
  * [Rough idea](#idea)
  * [A first example: factorial](#example1)
  * [A second example: canopy](#example2)
  * [Observing evaluation order](#evalorder)
  * [Relating continuations and monads](#cpsmonad)

* [Programming with success and failure continuations](#succfail)
  * [Finding an element in a list](#findcps)
  * [Finding the index of a leaf in a binary tree](#findLeafIndex)
  * [A backtracking SAT solver](#satsolver)

* [References](#references)


<a name="cps"></a>
# Continuation-passing style

<a name="background"></a>
## Background

The concept of *continuation-passing style* has been [rediscovered multiple times](https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/reynolds-discoveries.pdf) in computer science, in different forms:

* as a versatile style of programming in functional languages, capable of expressing fine-grained patterns of control flow;
* as a way of giving precise mathematical semantics to various features of programming languages such as goto statements;
* as a technique for compiling functional languages down towards low-level machine code.

We are going to focus on the first aspect, i.e., on continuation-passing style (or CPS) as a particular style of functional programming, although we may also give some hints as to the second and third aspects.


CPS has a variety of fascinating connections to topics outside of computer science as well, such as to "double-negation translation" in logic, and to "type raising" in linguistics &mdash; if you are interested in any of that,  you can find out more in the [references](#references) at the bottom of these notes.

<a name="idea"></a>
## Rough idea

Any function you write in a functional language can be rewritten in continuation-passing style.
This involves the following recipe:

1. pass an extra argument to the function; this argument is called the "continuation", and is itself a function (so now your original function becomes a *higher-order function*, if it was not already)
2. whenever the original function returned a value, instead call the continuation with that value
3. whenever the original function made a recursive call and did something with the result, call the new function with an updated continuation that gives a name to the result and continues the computation

Here we've assumed a single recursive function, but this rough recipe can be adapted to mutually recursive functions, or to convert an entire program into CPS.

As we will see later, continuation-passing style also gives us a means of writing *new* functions that we did not necessarily know how to write before.
But to get the basic idea first, let's start with a toy example.

<a name="example1"></a>
## A first example: factorial
 
Recall the factorial function:
```haskell
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)
```

Here is the version in continuation-passing style:

```haskell
factcps :: Integer -> (Integer -> r) -> r
factcps 0 k = k 1
factcps n k = factcps (n-1) (\m -> k (n * m))
```

What have we done here?
Let's break it down:

1. Following the first step of the rough recipe, we began by giving the function an extra argument, replacing the original type
   `Integer -> Integer` of `fact` by the higher-order type `Integer -> (Integer -> r) -> r` of `factcps`.
   Here `r` is a type parameter and can be instantiated to an arbitrary type, in other words the type of `fastcps` is polymorphic.
   (The letter 'r' stands for "return type", and is often used when speaking about continuations.)

2. Following the second step of the rough recipe, we replaced the clause `fact 0 = 1` by the clause `factcps 0 k = k 1`.
   Here we use the letter `k` as the name for the extra continuation parameter (again this is typical), and following the recipe, in this case where the input integer is 0, instead of directly returning the value 1 we instead pass it to the continuation.

3. Following the third step of the rough recipe, we replaced the clause `fact n = n * fact (n-1)` by the clause `factcps n k = factcps (n-1) (\m -> k (n * m))`.
   This requires the most explanation.
   In the original definition, we made a recursive call to `fact (n-1)` and then multiplied the result by `n`:
   one can say that `(n *)` is the *surrounding context* of the original recursive call.
   In the CPS version, this context is composed with the continuation parameter `k` to construct the new continuation `\m -> k (n * m)`, which is passed as the second argument in the recursive call to `factcps`.

To better understand what's going on, let's try to give a [rigorous specification](rigour.md) of the higher-order function computed by `factcps`.
With a bit of thought, we see that `factcps n k` should satisfy the property that
```hs
factcps n k = k (fact n)
```
for all natural numbers `n :: Integer` and functions `k :: Integer -> r`, in other words, that it ends up being equivalent to calling the continuation with the value of the factorial.
In particular, we should have that
```hs
factcps n id = id (fact n) = fact n
```
where `id = \x -> x` is the *identity continuation*.
(Remember the identity continuation: it will come up all the time in what we're doing.)

We can try to convince ourselves of this property by testing various examples, but in this case we can even do better and *prove* it by induction.
Since the proof is short and quite instructive, we go through it here.

**Proof that `factcps n k = k (fact n)`, by induction on `n`:**

* case `n = 0`:
  ```
    factcps 0 k
  =              [by definition of factcps]
    k 1
  =              [by definition of fact]
    k (fact 0)
  ```

* case `n > 0`:
  ```
    factcps n k
  =                                  [by definition of factcps]
    factcps (n-1) (\m -> k (n * m))
  =                                  [by induction hypothesis]
    (\m -> k (n * m)) (fact (n-1))
  =                                  [by beta reduction]
    k (n * fact (n-1))
  =                                  [by definition of fact]
    k (fact n)
  ```
  Observe that on the third line we applied the induction hypothesis at `n-1`, taking the new continuation to be `\m -> k (n * m)` (so the continuation "grew" from the original `k`, but that's okay because the induction is on `n` rather than `k`).
  On the fourth line we applied the so-called *beta reduction* rule of lambda calculus, meaning that we substituted the value `fact (n-1)` for the variable `m` in `k (n * m)`.

<a name="example2"></a>
## A second example: canopy

With the toy example of factorial, it's not really obvious what advantages we gain from rewriting a function in continuation-passing style.
To get a bit more of a feeling for the *usefulness* of CPS, let's consider the only slightly less trivial example of computing the canopy of a binary tree with labelled leaves.
First let's recall the usual version (also called "direct style" version):
```haskell
data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show,Eq)

canopy :: Bin a -> [a]
canopy (L x)   = [x]
canopy (B l r) = canopy l ++ canopy r
```

In this case, there are actually *two* different ways of rewriting `canopy` in CPS:
```haskell
canopycps  :: Bin a -> ([a] -> r) -> r
canopycps  (L x)   k = k [x]
canopycps  (B l r) k = canopycps l (\xs -> canopycps r (\ys -> k (xs ++ ys)))

canopycps' :: Bin a -> ([a] -> r) -> r
canopycps' (L x)   k = k [x]
canopycps' (B l r) k = canopycps' r (\ys -> canopycps' l (\xs -> k (xs ++ ys)))
```
Both versions have the same type `Bin a -> ([a] -> r) -> r`, and both deal with the `L x` case in the same way, namely by passing the list `[x]` to the continuation `k`.
However, the difference is in the treatment of the `B l r` case: although both of these versions are correct translations of the original recursive clause
```hs
canopy (B l r) = canopy l ++ canopy r
```
following our recipe for conversion to CPS, as we will see, the first version enforces *left-to-right evaluation order*, whereas the second version enforces *right-to-left evaluation order*.
So how did we derive these two different versions?

Well, in the first, "left-to-right" version, we focused on the recursive call `canopy l` and treated `(++ canopy r)` as the surrounding context, leading to the definition:
```hs
canopycps  (B l r) k = canopycps l (\xs -> canopycps r (\ys -> k (xs ++ ys)))
```
Whereas in the second, "right-to-left" version, we focused on the recursive call `canopy r` and treated `(canopy l ++)` as the surrounding context, leading to the definition:
```hs
canopycps' (B l r) k = canopycps' r (\ys -> canopycps' l (\xs -> k (xs ++ ys)))
```
In one case, we begin by computing the canopy of `l` (named `xs`), and then combine that with the canopy of `r` (named `ys`), whereas in the other case, we begin by computing the canopy of `r` and then combine that with the canopy of `l`.
In general, continuation-passing style provides a mathematically precise means of distinguishing "what comes first" from "what comes next".

Of course, in this example, it doesn't really make a difference whether we first compute the canopy `xs` of `l` and then combine that with the canopy `ys` of `r`, or vice versa: ultimately we obtain the same result `xs ++ ys`.
Indeed, both versions satisfy the same specification:
```hs
canopycps t k = canopycps' t k = k (canopy t)
```
for all trees `t :: Bin a` and functions `k :: [a] -> r`.

Again there is a short proof of this statement by induction on `t` (now it is an [induction on binary trees](data.md#bintreepf)).
We show only the argument that `canopycps t k = k (canopy t)`, since the argument that `canopycps' t k = k (canopy t)` (and hence `canopycps t k = canopycps' t k`) is symmetric.

**Proof that `canopycps t k = k (canopy t)`, by induction on `t`:**

* case `t = L x`:
  ```
    canopycps (L x) k
  =                      [by definition of canopycps]
    k [x]
  =                      [by definition of canopy]
    k (canopy (L x))
  ```

* case `t = B l r`:
  ```
    canopycps (B l r) k
  =                                                         [by definition of canopycps]
    canopycps l (\xs -> canopycps r (\ys -> k (xs ++ ys)))
  =                                                         [by induction hypothesis on l]
    (\xs -> canopycps r (\ys -> k (xs ++ ys))) (canopy l)
  =                                                         [by beta reduction]
    canopycps r (\ys -> k (canopy l ++ ys))
  =                                                         [by induction hypothesis on r]
    (\ys -> k (canopy l ++ ys)) (canopy r)
  =                                                         [by beta reduction]
    k (canopy l ++ canopy r)
  =                                                         [by definition of canopy]
    k (canopy (B l r))
  ```

<a name="evalorder"></a>
## Observing evaluation order

Since the two versions `canopycps` and `canopycps'` are functionally equivalent, how can we tell that one translation corresponds to left-to-right evaluation order, and the other to right-to-left evaluation order?
Indeed, what about the original direct style version of the function where we had:
```hs
canopy (B l r) = canopy l ++ canopy r
```
Given some input tree, can we tell whether its canopy will be computed from left-to-right or from right-to-left?
Actually, in general, answering this kind of question in Haskell is quite subtle, and requires us to know about the details of lazy evaluation.

On the other hand, evaluation order becomes more readily observable in the presence of *side-effects*, which in Haskell requires us to work inside the IO monad.
Suppose we instrument the `L x` case of our example so that it prints the value at the given leaf before passing its fringe `[x]` to the continuation:
```hs
... = putStrLn ("visited: " ++ show (L x)) >> k [x]
-- or equivalently:
{-
... = do
        putStrLn ("visited: " ++ show (L x))
        k [x]
-}
```
To do this we need to restrict the return type of the continuation to the IO monad, and also assume that the type of values is `Show`able, so the signature of the CPS canopy functions becomes slightly less polymorphic:
```hs
... :: Show a => Bin a -> ([a] -> IO r) -> IO r
```
Otherwise, though, everything else stays the same, and we call these instrumented versions `canopycpsIO` and `canopycpsIO'`:
```haskell
canopycpsIO :: Show a => Bin a -> ([a] -> IO r) -> IO r
canopycpsIO (L x)   k = putStrLn ("visited " ++ show x) >> k [x]
canopycpsIO (B l r) k = canopycpsIO l (\xs -> canopycpsIO r (\ys -> k (xs ++ ys)))

canopycpsIO' :: Show a => Bin a -> ([a] -> IO r) -> IO r
canopycpsIO' (L x)   k = putStrLn ("visited " ++ show x) >> k [x]
canopycpsIO' (B l r) k = canopycpsIO' r (\ys -> canopycpsIO' l (\xs -> k (xs ++ ys)))
```
By running these on the same input, we can observe that the left-to-right and the right-to-left versions will produce different outputs on the way to computing the same final answer:
```hs
> canopycpsIO  (B (B (L 1) (L 2)) (L 3)) return
visited 1
visited 2
visited 3
[1,2,3]
> canopycpsIO' (B (B (L 1) (L 2)) (L 3)) return
visited 3
visited 2
visited 1
[1,2,3]
```
(Notice that here we passed `return :: [a] -> IO [a]` as the initial continuation, rather than `id`.)

So we see that rewriting the canopy function in continuation-passing style has given us more fine-grained control over the flow of execution, in letting us choose explicitly between left-to-right and right-to-left evaluation.
(This example also hints at one of the reasons why continuation-passing style is useful when studying the *semantics* of programming languages, since it can be used to explicate concepts such as evaluation order.)

<a name="cpsmonad"></a>
## Relating continuations and monads

The previous motivation for CPS may have reminded you of [monads](monads.md) &mdash; indeed, there are many similarities!
Compare the (uninstrumented) left-to-right CPS translation of the canopy function:
```hs
canopycps :: Bin a -> ([a] -> r) -> r
canopycps (L x)   k = k [x]
canopycps (B l r) k = canopycps l (\xs -> canopycps r (\ys -> k (xs ++ ys)))
```
to the analogous monadic translation:
```haskell
canopymon :: Monad m => Bin a -> m [a]
canopymon (L x)   = return [x]
canopymon (B l r) = canopymon l >>= \xs -> canopymon r >>= \ys -> return (xs ++ ys)
```
They look even more similar if we use the infix application operator
```hs
($) :: (a -> b) -> a -> b
```
to get rid of some pairs of parentheses in the CPS version:
```hs
canopycps (B l r) k = canopycps l $ \xs -> canopycps r $ \ys -> k (xs ++ ys)
```
And now here's the monadic version in "do notation":
```hs
canopymon (B l r) = do
  xs <- canopymon l
  ys <- canopymon r
  return (xs ++ ys)
```
compared with a reformatted copy of the CPS version:
```hs
canopycps (B l r) k =
  canopycps l $ \xs ->
  canopycps r $ \ys ->
  k (xs ++ ys)
```
In slightly different notations, both the monadic and the CPS versions express the idea of: "*run* some computation, *name* the result, and *continue* with the rest of the computation".
In general, some of the motivations for using continuation-passing style are similar to the motivations for using monadic style, and often one can choose one or the other, although there are situations where it is more natural to use continuations over monads and vice versa.

There is also the so-called *continuations monad*:

```haskell
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  -- fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f xm = Cont (\k -> runCont xm (k . f))

instance Applicative (Cont r) where
  -- pure :: a -> Cont r a
  pure x = Cont (\k -> k x)
  -- (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  fm <*> xm = Cont (\k -> runCont fm (\f -> runCont xm (\x -> k (f x))))

instance Monad (Cont r) where
  -- return :: a -> Cont r a
  return x = Cont (\k -> k x)
  -- (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  xm >>= g = Cont (\k -> runCont xm (\x -> runCont (g x) k))
```

It is worth knowing about its existence, although you do not have to memorise these definitions.
One reason that the continuations monad is interesting from a purely logical standpoint is that the underlying type `(a -> r) -> r` can be read as a sort of "double-negation" of `a`.
This is based on a reading of the function type constructor `(->)` as the logical connective "implies", and the return type `r` as the logical proposition "false".

A reason that the continuations monad is interesting from a programming standpoint is that it allows us to implement so-called "control operators", including the following operations:
```haskell
abort :: r -> Cont r a
abort x = Cont (\k -> x)

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC xm = Cont (\k -> runCont (xm (\x -> abort (k x))) k)
```
(Again, it is not so important for you to know this, although you can find out more in the [references](#references) if you are interested.)

<a name="succfail"></a>
# Programming with success and failure continuations

One way of increasing the expressive power of continuation-passing style is to have more than one continuation.
For example, a common pattern is to have *two* continuations: one called the "success" continuation and invoked in the case that the function successfully computes an answer, the other called the "failure" continuation and invoked in the case that there is no good answer.
As we will see, this pattern can be used to implement exception handling and backtracking, among other things.


<a name="findcps"></a>
## Finding an element in a list

```haskell
findcps :: (a -> Bool) -> [a] -> (a -> r) -> r -> r
findcps p []     succ fail = fail
findcps p (x:xs) succ fail
               | p x       = succ x
               | otherwise = findcps p xs succ fail
```
Observe that the success continuation takes a value of type `a` as argument, whereas the failure continuations takes no arguments.

**Exercise**:
By choosing appropriate success and failure continuations, use `findcps` to implement the two different versions of the `find` function mentioned in the lecture on [rigorous programming](rigour.md):
```haskell
find1 :: (a -> Bool) -> [a] -> a
find2 :: (a -> Bool) -> [a] -> Maybe a
```
<details><summary>(Answer for find1)</summary>

```haskell
find1 p xs = findcps p xs id undefined
```

</details>
<details><summary>(Answer for find2)</summary>

```haskell
find2 p xs = findcps p xs Just Nothing
```

</details>

<a name="findLeafIndex"></a>
## Finding the index of a leaf in a binary tree

```haskell
findLeafIndex :: (a -> Bool) -> Bin a -> (Int -> r) -> (Int -> r) -> r
findLeafIndex p (L x) succ fail
                      | p x       = succ 0
                      | otherwise = fail 1
findLeafIndex p (B l r) succ fail = findLeafIndex p l succ
                                                      (\n -> findLeafIndex p r (succ . (n+)) (fail . (n+)))
```
Observe that both the success and failure continuations take an `Int` as argument: in the success case, this is expected to be the index of the first leaf satisfying the predicate, numbering the leaves from left to right (starting at 0), whereas in the failure case, this is expected to be the total number of leaves in the binary tree.

Examples:
```hs
> findLeafIndex (\n -> odd n && n > 1) (B (B (L 1) (L 2)) (L 3)) id (\n -> error ("not found within " ++ show n ++ " leaves"))
2
> findLeafIndex (\n -> odd n && n < 1) (B (B (L 1) (L 2)) (L 3)) id (\n -> error ("not found within " ++ show n ++ " leaves"))
*** Exception: not found within 3 leaves
CallStack (from HasCallStack):
  error, called at <interactive>:232:74 in interactive:Ghci4
```

Note that a similar functionality can be achieved using the state monad.

<a name="satsolver"></a>
## A backtracking SAT solver

Found in [these notes](satsolver.md).

<a name="references"></a>
# References

If you are interested in learning more on your own, the references below go deeper into various aspects of continuations.
Some are quite advanced, and in no way are these required reading.

On the history of continuations in computer science (and specifically programming languages theory):
* John C. Reynolds, [The Discoveries of Continuations](https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/reynolds-discoveries.pdf)

On compiling functional languages using continuation-passing style:
* Andrew Appel, [Compiling with Continuations](https://dl.acm.org/citation.cfm?id=1512932)
* Matthew Might, [How to compile with continuations](http://matt.might.net/articles/cps-conversion/)

On continuations in logic:
* Timothy Griffin, [A formulae-as-types notion of control](https://www.cl.cam.ac.uk/~tgg22/publications/popl90.pdf)
* Philip Wadler, [Call-by-value is dual to call-by-name](https://homepages.inf.ed.ac.uk/wadler/papers/dual/dual.pdf)

On continuations in linguistics:
* Chris Barker, [Continuations and the nature of quantification](https://www.semanticsarchive.net/Archive/902ad5f7/barker.continuations.pdf)
* Chris Barker, [Continuations in Natural Language](http://www.cs.bham.ac.uk/~hxt/cw04/barker.pdf)

