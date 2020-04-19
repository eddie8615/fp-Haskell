# Lazy evaluation and infinite data structures

# Contents

* [Evaluation strategies](#eval)
  * [Evaluation, observational equivalence, and efficiency](#eval1)
  * [Call-by-value vs call-by-name](#cbvcbn)
  * [Termination](#termination)
  * [Laziness = delayed evaluation + sharing](#laziness)
  * [The cost of laziness](#costlaziness)
  * [Profiling with :sprint and trace](#profiling)

* [Infinite data](#infinite)
  * [Infinite lists (a.k.a. streams)](#streams)
  * [State machines](#machines)

<a name="eval"></a>
# Evaluation strategies

<a name="eval1"></a>
## Evaluation, observational equivalence, and efficiency

Many programming languages have some notion of *evaluation*, which is the process of reducing a potentially complex program expression into a simpler *value*, or diagrammatically:
```         
             evaluation
expression ~~~~~~~~~~~~~~~> value
```
For example, the expressions `4+1` and `3+2` both evaluate to `5`, while the expressions `"hell" ++ "o"` and `reverse "olleh"` both evaluate to `"hello"`.
Haskell encourages "pure" functional programming, in the sense that usually expressions *only* reduce to values and don't perform any other side-effects.
Although as we have seen, even in Haskell it is possible for evaluating an expression to have some side-effects (e.g., printing to the screen using the IO monad), so that the above diagram should really be refined to something like so:
```         
             evaluation
expression ~~~~~~~~~~~~~~~> value
            [side-effects]
```
And actually, it's also possible for an expression to never produce a value because it never finishes evaluating:
```         
             evaluation
expression ~~~~~~~~~~~~~~~> value or non-termination
            [side-effects]
```
In any case, the key feature that distinguishes *functional* programming languages is that functions themselves can be computed as values.
In this lecture, we will learn a bit about how evaluation works in functional programming languages in general and in Haskell in particular.

As a programmer, having an accurate mental model of evaluation is important, both for reasoning about the correctness of your programs and for improving their efficiency.
In terms of correctness, two expressions that evaluate to the same value (and produce the same side-effects) are in a certain sense interchangeable: one expression can be replaced by the other without affecting the input-output behavior of a program.
For example, if `f` is a function of type `(Integer, String) -> Bool`, then we know that the expressions
```hs
f (4+1, "hell" ++ "o")
```
and
```hs
f (3+2, reverse "olleh")
```
will both evaluate to the same boolean value, regardless of the definition of `f`.
More generally, even when two expressions may not evaluate to literally the same value, they may be nonetheless *observationally equivalent*, in the sense that there is no way to distinguish them by any finite observation within the language.
Here, what kinds of "observations" are allowed depends upon the type of value.
For example, a function value can be observed by applying it to some argument and observing the result.
This means that two expressions of type `a -> b` are observationally equivalent just in case they evaluate to equivalent values of type `b` when applied to any input of type `a`.

As an example of observational equivalence, consider the following definitions:
```haskell
f1,f2,f3 :: Integer -> Integer
f1 = \n -> foldr (+) 0 [1..n]
f2 = \n -> foldl (+) 0 [1..n]
f3 = \n -> (n * (n+1)) `div` 2
```
All three definitions implement the same function from inputs to outputs (at least restricting to the intended inputs, namely integers `n >= 0`).
The fact that the first two implementations are equivalent `f1 n = f2 n` for all inputs `n >= 0` relies on the associativity and unit laws for addition.
The fact that the last is equivalent to the first two relies on the formula
```
                  n (n+1)
 1 + ... + n  =  ---------
                     2
```
that we know to be true from algebra (and which can be proved for example by induction on n).

On the other hand, the three implementations are different in terms of *efficiency*.
In real world programming, we often care about how much time, battery power, etc., a program uses, and not just it's input-output behavior.
To have any chance of predicting this in advance, we need to have a good idea of how evaluation works in our programming language.
For instance, in the above example, it's clear that the last version is much more efficient than the first two for large values of `n`, but what about the difference between using `foldr` and `foldl`?
To answer this, we need to know something of the details of evaluation in Haskell.

<a name="cbvcbn"></a>
## Call-by-value vs call-by-name

In the classification of programming languages, there is a basic dichotomy between languages with *call-by-value* and *call-by-name* evaluation for function application.
Languages with call-by-value evaluation include C, Java, and OCaml, among others.
Languages with call-by-name evaluation are more are, though as we will see later, the default evaluation strategy in Haskell is actually a hybrid between call-by-value and call-by-name, known as call-by-*need*.

Let's illustrate the distinction between call-by-value and call-by-name evaluation with an example (from Chapter 15 of Hutton), starting with the increment function `inc`:
```haskell
inc :: Int -> Int
inc n = n + 1
```
Under call-by-value, the argument of a function is always evaluated before applying the function.
This is also called **eager evaluation**.
So, the expression `inc (2*3)` is evaluated as follows under call-by-value evaluation:
```hs
   inc (2*3)
~~>             [ multiply 2 and 3 ]
   inc 6
~~>             [ apply inc ]
   6 + 1
~~>             [ add 6 and 1 ]
   7
```

Under call-by-name evaluation, the argument is passed unevaluated (a.k.a., by "name") to the function:
```hs
   inc (2*3)
~~>           [ apply inc ]
   (2*3) + 1
~~>           [ multiply 2 and 3 ]
   6 + 1
~~>           [ add 6 and 1 ]
   7
```

So in one evaluation strategy we do the multiplication first, and in the other we do the application first.
In this simple example, we see that the order-of-evaluation doesn't actually *matter* for the final result, which is 7 in both cases.
However, the difference between call-by-value and call-by-name becomes observable in the presence of side-effects, even when the only "side-effect" is non-termination.

<a name="termination"></a>
## Termination

Let's define a function that loops infinitely:

```haskell
loop :: Int -> Int
loop n = loop (n+1)
```

And another function that given an argument `x`, *returns* a constant function that ignores its argument and always returns `x`:
```haskell
const :: a -> b -> a
const x = \y -> x
```
Now consider evaluating the expression `(const 0) (loop 1)`.

Under call-by-value:
```hs
   (const 0) (loop 1)
~~>                     [ apply const ]
   (\y -> 0) (loop 1)
~~>                     [ apply loop ]
   (\y -> 0) (loop 2)
~~>                     [ apply loop ]
   (\y -> 0) (loop 3)
~~>                     [ ... ]
   ...
```
Under call-by-name:
```hs
   (const 0) (loop 1)
~~>                     [ apply const ]
   (\y -> 0) (loop 1)
~~>                     [ beta reduction ]
   0
```

We see that the expression evaluates to `0` under call-by-name evaluation, but fails to terminate under call-by-value evaluation.

> **Aside:**
> In most functional programming languages, there is never any evaluation "under a lambda" in either call-by-value or call-by-name evaluation strategies.
> That is, any lambda expression `\x -> e` is already considered a value, regardless of `e`.
> This means that it is always possible to delay the evaluation of an expression `e` by wrapping it inside a dummy function `\_ -> e`, called a "thunk".
> For example, following the above, the expression `(const 0) (\_ -> loop 1)` evaluates to `0`, even under call-by-value.
> More generally, it is possible to use thunks to simulate call-by-name evaluation within a call-by-value language.

<a name="laziness"></a>
## Laziness = delayed evaluation + sharing

In general, call-by-name has the nice property that if a program terminates and reduces to a value under *some* evaluation strategy, then it will terminate and reduce to that value under call-by-name evaluation.
In this sense, call-by-name is "more efficient" than call-by-value evaluation, at least in terms of termination.
On the other hand, as we will now illustrate, it is also "less efficient", in the sense that it can lead to a lot of needlessly duplicated computations.

Consider the following example (again from Hutton, Chapter 15), now using the squaring function:
```haskell
square :: Int -> Int
square n = n * n
```
We evaluate the expression `square (1+2)`.

Under call-by-value:
```hs
   square (1+2)
~~>               [ perform addition ]
   square 3
~~>               [ apply square ]
   3 * 3
~~>               [ perform multiplication ]
   9
```

Under call-by-name:
```hs
   square (1+2)
~~>               [ apply square ]
   (1+2) * (1+2)
~~>               [ perform addition ]
   3 * (1+2)
~~>               [ perform addition ]
   3 * 3
~~>               [ perform multiplication ]
   9
```

We see that under call-by-name evaluation the expression `1+2` is duplicated, leading us to perform the addition operation twice, whereas in call-by-value evaluation the addition is performed only once.

Call-by-need evaluation, also called **lazy evaluation**, is an attempt at addressing this shortcoming of call-by-name.
The basic idea is to give names to intermediate subexpressions to make sure that they only get evaluated at most once and can be shared across computation.
Here is an idealisation of how the above expression might get evaluated under call-by-need:
```hs
   square (1+2)
~~>                         [ introduce a name x for the subexpression ]
   let x = 1+2 in square x
~~>                         [ apply square ]
   let x = 1+2 in x * x
~~>                         [ perform addition (needed to make progress) ]
   let x = 3 in x * x
~~>                         [ fetch value of x ]
   let x = 3 in 3 * 3
~~>                         [ perform multiplication ]
   let x = 3 in 9
~~>                         [ garbage collect x ]
   9
```

<a name="costlaziness"></a>
## The cost of laziness

In a certain sense, call-by-need is "more efficient" than both call-by-value and call-by-name, as we can summarise roughly as follows:

* under call-by-value, the argument of a function is evaluated *exactly once*;
* under call-by-name, the argument of a function is evaluated *any number of times* (in particular, possibly zero times);
* under call-by-need, the argument of a function is evaluated *at most once*.

On the other hand, there is also a hidden cost to lazy evaluation, which is all of the extra bookkeeping work needed to ensure that expressions are not evaluated until they are needed.
In practice, this can actually lead to a substantial overhead relative to eager evaluation, particularly in terms of memory consumption.

Haskell has one basic mechanism for forcing eager evaluation, which is a function
```hs
seq :: a -> b -> b
```
that evaluates its first argument before returning its second argument.
(Well, kind of.  See [here](https://wiki.haskell.org/Seq) for a more precise explanation of what `seq` is doing.)
The `seq` function can be used to encode other operations, such as a call-by-value function application operator
```hs
($!) :: (a -> b) -> a -> b
```
which can be defined by
```hs
f $! x = x `seq` fx
```

<a name="profiling"></a>
## Profiling with :sprint and trace

These are useful for understanding how your program is being evaluated at a low level.

Example with `:sprint`:
```hs
*Main> let x = 1 * 2 :: Int
*Main> let y = x + 3 :: Int
*Main> let z = x + y * y
*Main> :sprint x
x = _
*Main> :sprint y
y = _
*Main> :sprint z
z = _
*Main> z
27
*Main> :sprint z
z = 27
*Main> :sprint y
y = 5
*Main> :sprint x
x = 2
*Main>
```

Example with `trace`:
```hs
*Main> import Debug.Trace
*Main Debug.Trace> let x = trace "x!" (1 * 2) :: Int
*Main Debug.Trace> let y = trace "y!" (x + 3) :: Int
*Main Debug.Trace> let z = trace "z!" (x + y * y)
*Main Debug.Trace> z
z!
x!
y!
27
*Main Debug.Trace> let x = trace "x!" (1 * 2) :: Integer
*Main Debug.Trace> let y = trace "y!" (x + 3) :: Integer
*Main Debug.Trace> let z = trace "z!" (x + y * y)
*Main Debug.Trace> z
z!
y!
x!
27
*Main Debug.Trace> 
```
(Observe that after we switched from machine integers to arbitrary-precision integers, GHC decided to switch from left-to-right evaluation order to right-to-left evaluation order!  In general, predicting these kinds of things in advance can be difficult in Haskell.)

Here's another example, now considering traced versions of the functions `canopy`, `canopycps`, and `canopycps'` that we wrote in the lecture on [continuations](continuations.md#example2):
```hs
*Main Debug.Trace> canopy (L x) = trace ("visited " ++ show x) [x] ; canopy (B l r) = canopy l ++ canopy r
*Main Debug.Trace> canopycps (L x) k = trace ("visited " ++ show x) (k [x]) ; canopycps (B l r) k = canopycps l (\xs -> canopycps r (\ys -> k (xs ++ ys)))
*Main Debug.Trace> canopycps' (L x) k = trace ("visited " ++ show x) (k [x]) ; canopycps' (B l r) k = canopycps' r (\ys -> canopycps' l (\xs -> k (xs ++ ys)))
*Main Debug.Trace> 
*Main Debug.Trace> t = B (L 1) (B (L 2) (L 3))
*Main Debug.Trace> canopycps t id
visited 1
visited 2
visited 3
[1,2,3]
*Main Debug.Trace> canopycps' t id
visited 3
visited 2
visited 1
[1,2,3]
*Main Debug.Trace> canopy t
visited 1
[1visited 2
,2visited 3
,3]
```

<a name="infinite"></a>
# Infinite data

<a name="streams"></a>
## Infinite lists (a.k.a. streams)

### nats, evens, odds

We consider a few different ways of defining the natural numbers, the even numbers, and the odd numbers, as infinite lists.

**Version #1**
```haskell
upFrom :: Integer -> [Integer]
upFrom n = n : upFrom (n + 1)

nats,evens,odds :: [Integer]
nats = upFrom 0
evens = map (*2) nats
odds = map (+1) evens
```

```hs
*Main Debug.Trace> :sprint nats
nats = _
*Main Debug.Trace> take 5 odds
[1,3,5,7,9]
*Main Debug.Trace> :sprint nats
nats = 0 : 1 : 2 : 3 : 4 : _
*Main Debug.Trace> :sprint evens
evens = 0 : 2 : 4 : 6 : 8 : _
*Main Debug.Trace> :sprint odds
odds = 1 : 3 : 5 : 7 : 9 : _
*Main Debug.Trace> 
```

**Version #2**

```haskell
interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs

nats',evens',odds' :: [Integer]
evens' = 0 : map (+1) odds'
odds'  = map (+1) evens'
nats'  = interleave evens' odds'
```

```hs
*Main Debug.Trace> :sprint nats'
nats' = _
*Main Debug.Trace> take 5 nats'
[0,1,2,3,4]
*Main Debug.Trace> :sprint evens'
evens' = 0 : 2 : 4 : _
*Main Debug.Trace> :sprint odds'
odds' = 1 : 3 : _
*Main Debug.Trace>
```

**Version #3**
```haskell
everyOther :: [a] -> [a]
everyOther (x:y:xs) = x:everyOther xs

nats'',evens'',odds'' :: [Integer]
nats''  = [0..]
evens'' = everyOther nats''
odds''  = everyOther (tail nats'')
```

```hs
*Main Debug.Trace> :sprint nats''
nats'' = _
*Main Debug.Trace> take 5 odds''
[1,3,5,7,9]
*Main Debug.Trace> :sprint nats''
nats'' = 0 : 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : _
```

### Fibonacci numbers

**Version #1**

```haskell
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
```

**Version #2**

```haskell
fib' :: [Integer]
fib' = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)
```

<a name="machines"></a>
## State machines

Consider a state machine that takes transitions on inputs of type `i`, and has a current output of type `o` (technically, this is called a [Moore machine](https://en.wikipedia.org/wiki/Moore_machine)):

```haskell
data Machine i o = M { out :: o, step :: i -> Machine i o }

run :: Machine i o -> [i] -> o
run m []     = out m
run m (x:xs) = run (step m x) xs
```

A value of type `Machine i o` can be thought of as a kind of infinite tree, with nodes labelled by the value `out :: o`, and the transition function `step :: i -> Machine i o` describing how to compute children.
Concretely, though, these infinite trees can be given finite descriptions as *cyclically-defined values*, just like we saw above with the recursive definition of different infinite lists of numbers.

Here is an example of a machine with two states, defined as two mutually recursive values of type `Machine Bit Bool`:
```haskell
data Bit = B0 | B1  deriving (Show,Eq)

s1,s2 :: Machine Bit Bool
s1 = M { out = True,  step = \b -> case b of B0 -> s2 ; B1 -> s1 }
s2 = M { out = False, step = \b -> case b of B0 -> s1 ; B1 -> s2 }
```
Can you see how this machine works, and what *language* it recognizes via the `run` function?
```hs
> run s1 []
True
> run s1 [B0]
False
> run s1 [B1]
True
> run s1 [B0,B0]
True
> run s1 [B0,B1]
False
> run s1 [B1,B0]
False
> run s1 [B1,B1]
True
```
