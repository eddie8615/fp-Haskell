## Monads in Haskell


* The notion of [monad](https://en.wikipedia.org/wiki/Monad_(category_theory)) comes from a branch of mathematics called [category theory](https://en.wikipedia.org/wiki/Category_theory) (1940's). There is a profusion of monads in mathematics.

* In computer science they probably occurred first with the work of [Eugenio Moggi](https://en.wikipedia.org/wiki/Eugenio_Moggi) on [programming language semantics](https://en.wikipedia.org/wiki/Semantics_(computer_science)), to model [effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science)) (late 1980's, at the University of Edinburgh).

State, exceptions, IO, non-determinism, probabilistic computation, and more.

* Shortly after that, [Phil Wadler](https://en.wikipedia.org/wiki/Philip_Wadler), based on Moggi's work,
  discovered that they are a useful [programming technique](https://en.wikipedia.org/wiki/Monad_(functional_programming)).

* Then they were introduced in Haskell (for IO, in the language
  itself, and for other purposes, in the Haskell libraries).

## Unfortunately


* The Haskell library developers redefined monads recently, by factoring the `Monad` abstraction via `Functor` and `Applicative`.

* This breaks existing code. (But we will discuss how to fix it.)

* This invalidates most Haskell-monad tutorials on the web, and
  published books and research papers. (But here we will discuss how to understand them.)

* And it has the virtue of making things as complicated as possible for students. To make things simpler, or at least less complicated, we will first study the old-style version, which is directly based on traditional category theory, and then explain the factorization via `Functor` and `Applicative`.

* The new edition of our textbook has the "new style" of monads.

* I will teach the original style first, due to its relative simplicity.

* Then I will discuss how this has been made more abstract in the
  new version of the Haskell libraries.

## Examples of monads

Before discussing the definition, let's discuss some examples:


* Maybe.
* Lists.



## Writing function application backwards

```hs
   f :: a -> b
```
Feed the input `x :: a`, get the output `y :: b`:
```
           +-------+
   x --->  |   f   | ---> y
           +-------+
   y = f x
```
In Haskell we can write this as follows:
```haskell
infixl 6 --->
(--->) :: a -> (a -> b) -> b
x ---> f  =  f x
```
Then we can form a pipeline such as "feed `x` to `f`, then do `g` then do `h`":
```
   x ---> f ---> g ---> h = h(g(f x))
```


## Maybe

What if somewhere in a pipeline we fail to have a result to proceed?

Define a new feeding operator:
```haskell
infixl 6 -->

(-->) :: Maybe a -> (a -> Maybe b) -> Maybe b
xm --> f = case xm of
             Nothing -> Nothing
             Just x  -> f x
```
If we have
```hs
   xm :: Maybe a
   f  :: a -> Maybe b
   g  :: b -> Maybe c
   h  :: c -> Maybe d
```
we can get a value of type `Maybe d` with the pipeline
```
   xm --> f --> g --> h
```
Unfolding the definitions, this amounts to
```hs
   case xm of
     Nothing -> Nothing
     Just x  -> case f x of
                  Nothing -> Nothing
                  Just y  -> case g y of
                               Nothing -> Nothing
                               Just z  -> h z
```

Example: Maybe I have an int `x` and maybe I have an int
`y`. What is their sum if I have them?


With case:
```hs
   add :: Maybe Int -> Maybe Int -> Maybe Int
   add xm ym =
     case xm of
       Nothing -> Nothing
       Just x  -> case ym of
                     Nothing -> Nothing
                     Just y  -> Just(x+y)
```

With pattern matching:
```hs
   add1 :: Maybe Int -> Maybe Int -> Maybe Int
   add1 Nothing  Nothing  = Nothing
   add1 (Just x) Nothing  = Nothing
   add1 Nothing  (Just y) = Nothing
   add1 (Just x) (Just y) = Just(x+y)
```
What the program "really does" gets lost in a myriad of exceptional
details. In this particular case, we can simplify it to
```hs
   add1' :: Maybe Int -> Maybe Int -> Maybe Int
   add1' (Just x) (Just y) = Just(x+y)
   add1' _        _        = Nothing
```
but this isn't always the case.

With our feeding operator, `add` becomes
```
   add2 :: Maybe Int -> Maybe Int -> Maybe Int
   add2 xm ym = xm --> (\x -> ym --> (\y -> Just(x+y)))
```
With monads and bind (`>>=`) and return notation:
```hs
   add3 :: Monad m => m Int -> m Int -> m Int
   add3 xm ym = xm >>= (\x -> ym >>= (\y -> return(x+y)))
```
We can indent this as follows, to get close to `do`-notation.
```hs
   add3 :: Monad m => m Int -> m Int -> m Int
   add3 xm ym =
     xm >>= (\x ->
     ym >>= (\y ->
     return(x+y)))
```
With monads and `do`-notation:
```hs
   add4 :: Monad m => m Int -> m Int -> m Int
   add4 xm ym = do
     x <- xm
     y <- ym
     return(x+y)
```

The code is more readable, and, more importantly, more general.
It works for any monad `m`, not just `m = Maybe`

Summary: in this example, the monad provides automatic exception handling.

## List

What if we have functions that return multiple answers, in a list, and we want to make a pipeline out of them?

Define a new feeding operator:
```
   infixl 6 -->

   (-->) :: [a] -> (a -> [b]) -> [b]
   xs --> f = concat(map f xs)
```
If we map `f` to `xs`, we get a list of lists. We concatename them.

If we have
```hs
   xs :: [a]
   f  :: a -> [b]
   g  :: b -> [c]
   h  :: c -> [d]
```
we can get a value of type `[d]` with the pipeline
```
   xs --> f --> g --> h
```

## Monads - old style

```hs
   class Monad m where
      return :: a -> m a
      (>>=)  :: m a -> (a -> m b) -> m b
      (>>)   :: m a -> m b -> m b
      xm >> ym  =  xm >>= \_ -> ym -- default

```
Here m is a type constructor, which means that given a type `a`, we get the type `m a`.

### Maybe monad

For `m = Maybe` we want
```hs
   return x = Just x

   xm  >>= f = case xm of
                 Nothing -> Nothing
                 Just x  -> f x
```
Equivalently, we can use pattern matching to define `>>=`:
```hs
   Nothing >>= f  =  Nothing
   Just x  >>= f  =  f x
```
The full definition of the `Maybe` monad is
```hs
   instance Monad Maybe where
      return = Just

      Nothing >>= f  =  Nothing
      Just x  >>= f  =  f x
```

### List monad

It is pre-defined by
```hs
   instance Monad [ ] where
      return x = [x]
      xs >>= f = concat(map f xs)
```
Notice that `concat(map f xs)` is equivalent to `[y | x <- xs, y <- f x]`.

Example (again): The following are two different notations for the same thing:
```hs
   add3 :: Monad m => m Int -> m Int -> m Int
   add3 xm ym =
     xm >>= (\x ->
     ym >>= (\y ->
     return(x+y)))

   add4 :: Monad m => m Int -> m Int -> m Int
   add4 xm ym = do
     x <- xm
     y <- ym
     return(x+y)
```
The `do` notation with binder `<-` is syntax sugar for `>>=` and `\ ... -> `, and `add4` is just another way to write the same thing as in `add3`, which we could indent as
```hs
   add3 :: Monad m => m Int -> m Int -> m Int
   add3 xm ym =      -- do
     xm >>= (\x ->   --   x <- xm
     ym >>= (\y ->   --   y <- ym
     return(x+y)))   --   return(x+y)
```
to make this clearer. Incidentally, list-comprehension notation is syntax sugar for the list monad as well, and we will explain this later.

## Maybe and list

Now this code works for both the Maybe and list monads:
```hs
   *Main> add3 (Just 3) (Just 7)
   Just 10

   *Main> add3 Nothing (Just 7)
   Nothing

   *Main> add3 [3,30] [7,70]
   [10,73,37,100]

   *Main> add3 [] [7,70]
   []
```

And of course we could have used `add4` which is just a different way of writing `add3`.

## Summary so far

Monads in old style - just this:

```hs
   class Monad m where
     return :: a -> m a
     (>>=)  :: m a -> (a -> m b) -> m b
```
We have two examples (instances) of this at the moment: Maybe and list.

## Monads - new style

As in our textbook:

* First define `Functor`.

* Functors have `fmap` generalizing `map` from lists.

* Then define `Applicative`.

* An `Applicative` is a `Functor` with extra stuff.


  * `pure` (corresponding to `return`).
  * `<*>` which I will explain in due course.


* A `Monad` is an `Applicative` with extra stuff. This is `return` and `>>=`.

## Summary of the differences

### Old style - just this:
```hs
   class Monad m where
      return :: a -> m a
      (>>=)  :: m a -> (a -> m b) -> m b
```

### New style - all this:
```hs
   class Functor f where
      fmap :: (a -> b) -> f a -> f b

   class Functor f => Applicative f where
      pure  :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

   class Applicative m => Monad m where
      return :: a -> m a
      (>>=)  :: m a -> (a -> m b) -> m b
      return = pure -- default definition (can be overriden).
```

### The two examples - new style

#### Functor

```hs
   instance Functor Maybe where
      -- fmap :: (a -> b) -> Maybe a -> Maybe b
      fmap g Nothing  = Nothing
      fmap g (Just x) = Just(g x)


   instance Functor [] where
      -- fmap :: (a -> b) -> [a] -> [b]
      fmap = map
```

#### Applicative

```hs
   instance Applicative Maybe where
      -- pure :: a -> Maybe a
      pure = Just

      -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
      Nothing  <*> xm   = Nothing
      (Just g) <*> xm   = fmap g xm


   instance Applicative [] where
      -- pure :: a -> [a]
      pure x = [x]

      -- (<*>) :: [a -> b] -> [a] -> [b]
      gs <*> xs = [g x | g <- gs, x <- xs]
```

#### Monad

```hs
    instance Monad Maybe where
       Nothing >>= f  =  Nothing
       Just x  >>= f  =  f x


    instance Monad [] where
       xs >>= f = concat(map f xs)
```

## How to migrate from old style to new style code

If you already have a monad `M` in old style, to get it working
in new style use this boiler-plate code verbatim:
```hs
   instance Functor M where
      fmap f xm = xm >>= return . f

   instance Applicative M where
      pure = return
      fm <*> xm = fm >>= \f -> xm >>= return . f
 ```

What this means is that if we know `>>=` and `return`, then we can recover `fmap`, `pure` and `<*>` with the above definitions.

## Laws

* Functors, applicatives and monads are supposed to satisfy some laws.

* Check our textbook. These notes are not a replacement for the book, but only a quick guide.

# More examples of monads

We will consider a function `mfib` that will be tested with various
monads, and also modified to have *side-effects* on such monads.

## Running example of a monadic function

Consider the [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_number)
```
    1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89,...
```
viewed as a function:

  n   |   0 |   1 |  2  | 3 | 4 | 5 | 6  | 7  | 8  | 9  | 10 | ...
 -----|-----|-----|-----|---|---|---|----|----|----|----|----|----
 f(n) |   1 |   1 |   2 | 3 | 5 | 8 | 13 | 21 | 34 | 55 | 89 | ...

```haskell
f 0 = 1
f 1 = 1
f n = f(n-2) + f(n-1)
```
This *property* of `f` is also a *definition* of `f`, but not a very good one for computational purposes. We will explore this by first writing it in monadic form, and then using various different monads for different experiments.
```haskell
mfib :: (Eq a, Num a, Monad m) => a -> m a
mfib n =  if n == 0 || n == 1
          then return 1
          else do
                x <- mfib(n-1)
                y <- mfib(n-2)
                return(x + y)
```
Examples:
```
*Main> mfib 10
89
*Main> mfib 10 :: Maybe Int
Just 89
*Main> mfib 10 :: [Int]
[89]
```
The first example uses the `identity monad`, and we will define our own version of that monad.

## The identity monad

Our version of the identity monad has one constructor (corresponding
to `Just` in the `Maybe` monad), which we call `Return`:
```haskell
data Id a = Return a
            deriving (Show)

instance Functor Id where
  fmap f (Return x) = Return(f x)

instance Applicative Id where
  pure = Return
  (Return f) <*> (Return x) = Return(f x)

instance Monad Id where
  (Return x) >>= f = f x
```
Example:
```
*Main> mfib 10 :: Id Int
Return 89
```

## Tracing `mfib` with the `Writer` monad

```haskell
data Writer a = Result a String
                deriving Show


instance Monad Writer where
  return x = Result x ""
  xm >>= f = case xm of
               Result x s -> case f x of
                               Result y t -> Result y (s ++ t)

-- Boiler plate:

instance Functor Writer where
  fmap f xm = xm >>= return . f

instance Applicative Writer where
      pure = return
      xm <*> ym = xm >>= \x -> ym >>= return . x
```
Example:
```
*Main> mfib 10 :: Writer Int
Result 89 ""
```
But the whole point of the `Writer` monad is that we can write as a side-effect, while we compute (like print statements in imperative languages such as `Java` and `C`):
```haskell
write :: String -> Writer ()
write s = Result () s


wfib :: (Show a, Eq a, Num a) => a -> Writer a
wfib n = do
          write(show n ++ ", ") -- this is the only change
          if n == 0 || n == 1
          then return 1
          else do
                x <- wfib(n-1)
                y <- wfib(n-2)
                return(x + y)
```
Notice that now, because we are using a side-effect particular to the `Writer` monad, we can't work with an arbitrary monad `m` any longer.
```
*Main> wfib 10
Result 89 "10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 1, 2, 1, 0, 3, 2, 1, 0, 1, 4, 3, 2,
1, 0, 1, 2, 1, 0, 5, 4, 3, 2, 1, 0, 1, 2, 1, 0, 3, 2, 1, 0, 1, 6, 5, 4, 3, 2, 1,
0, 1, 2, 1, 0, 3, 2, 1, 0, 1, 4, 3, 2, 1, 0, 1, 2, 1, 0, 7, 6, 5, 4, 3, 2, 1, 0,
1, 2, 1, 0, 3, 2, 1, 0, 1, 4, 3, 2, 1, 0, 1, 2, 1, 0, 5, 4, 3, 2, 1, 0, 1, 2, 1,
0, 3, 2, 1, 0, 1, 8, 7, 6, 5, 4, 3, 2, 1, 0, 1, 2, 1, 0, 3, 2, 1, 0, 1, 4, 3, 2,
1, 0, 1, 2, 1, 0, 5, 4, 3, 2, 1, 0, 1, 2, 1, 0, 3, 2, 1, 0, 1, 6, 5, 4, 3, 2, 1,
0, 1, 2, 1, 0, 3, 2, 1, 0, 1, 4, 3, 2, 1, 0, 1, 2, 1, 0, "
```
That's lots of recursive calls to compute the 10th Fibonacci
number. The double recursive call causes an exponential number of
recursive calls. There will be a lecture on techniques to make this and
other function definitions with multiple recursive calls more
efficient.

## Counter monad

We can have a monad whose only available side-effect is to count.

```haskell
data Counter a = Count a Int
                 deriving Show


instance Monad Counter where
  return x = Count x 0
  xm >>= f = case xm of
               Count x c -> case f x of
                              Count y d -> Count y (c + d)

-- Boiler plate:

instance Functor Counter where
  fmap f xm = xm >>= return . f

instance Applicative Counter where
      pure = return
      xm <*> ym = xm >>= \x -> ym >>= return . x

-- Side-effect for this monad:

count :: Counter ()
count = Count () 1


cfib :: (Show a, Eq a, Num a) => a -> Counter a
cfib n = do
          count -- this is the only change
          if n == 0 || n == 1
          then return 1
          else do
                x <- cfib(n-1)
                y <- cfib(n-2)
                return(x + y)
```
We get the count f the recursive calls in addition to the result `89`:
```
*Main> cfib 10
Count 89 88
```

## The state monad

Allows to simulate variables in a storage. More abstractly, we have a state, and we can modify the state during the computation. If we are in a given state, we want to produce
 * a result, and
 * a new state.

The process that does that is called a *transition
function*. So we use the letter `T` for our constructor.

```haskell
data State s a = T(s -> (a,s))
```
We will use letters
  * `x`, `y`, `z` for values of type `a`,
  * `u`, `v`, `w` for states of type `s`,
  * `p`, `q` for transition functions of type `s -> (a,s)`.

The destructor (the function opposite to the constructor `T`) is traditionally called `runState`:
```haskell
runState :: State s a -> (s -> (a, s))
runState (T p) = p
```
Notice that it is not `State` which is the monad, but rather `State s` for a type `s` of states.
```haskell
instance Monad (State s) where
  return x = T(\u -> (x,u))
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  xm >>= f = case xm of
               T p -> T(\u -> case p u of
                                (x, v) -> case f x of
                                            T q -> q v)
```
 * The return function, given a value `x`, creates a transition function that doesn't change the current state `u`, and simply pairs `x` with it.

 * The bind function `>>=` is more complicated.

    * Given `xm :: State s a` and `f :: a -> State s b`, we need to produce something of type `State s b`.
    * We first extract the transition function from `xm` using `case` (we could have used `runState`). This is `p`.
    * Then, with the constructor `T`, we create the transition function of the result `State s a`.
    * Starting with the state `u`, we apply `p` to `u`.
    * This gives a pair, which we inspect with a `case`.
    * The first thing is a value `x`, which we give to `f`.
    * `f` produces a transition function `q`, which which we pass the state `v`.

We again need the boiler-plate code according to the new-style definition of monads in Haskell:
```haskell
instance Functor(State s) where
  fmap f xm = xm >>= return . f

instance Applicative(State s) where
      pure = return
      xm <*> ym = xm >>= \x -> ym >>= return . x
```
The above defines the monad. We also need to the define the three functions for side-effects.
This reads the state:
```haskell
get :: State s s
get = T(\s -> (s,s))
```
This replaces the state by a given state:
```haskell
put :: s -> State s ()
put s = T (\_ -> ((), s))
```
And this modifies the state by applying a function to it:
```haskell
modify :: (s -> s) -> State s ()
modify f = T(\s -> ((), f s))
```
And we modify our running example to use states. Our state holds a counter (if we wanted several variables, not just one, we could use pairs, tuples, lists etc.).
```haskell
sfib :: (Enum s, Eq a, Num a, Num b) => a -> State s b
sfib n = do
          modify succ -- this is the only line changed
          if n == 0 || n == 1
          then return 1
          else do
                x <- sfib(n-1)
                y <- sfib(n-2)
                return(x + y)
```
Example: run `sfib` with the input `10` and the initial state `0`, where we are using the state to count:
```
*Main> runState (sfib 10) 0
(89,177)
```
This means "run the function `sfib` with the input `10` on the initial state `0`".

Or we can do this in more traditional imperative style:
```haskell
sfib' :: (Enum s, Eq a, Num a, Num b) => a -> State s b
sfib' n = do
           u <- get       -- these are the only two
           put(succ u)    -- lines changed
           if n == 0 || n == 1
           then return 1
           else do
                 x <- sfib'(n-1)
                 y <- sfib'(n-2)
                 return(x + y)
```
This should behave in the same way:
```
*Main> runState (sfib' 10) 0
(89,177)
```

As another application of the state monad, you have seen that we can use accumulators to make the Fibonacci function more efficient. The use of accumulators amount to the very same thing as using the state monad. We can render this as follows. We have a state with "variables" `x` and `y`:

```haskell
ifib :: (Eq a, Num a, Num b) => a -> State (b, b) b

ifib 0 = do
          (x , y) <- get
          return x

ifib n = do
          (x , y) <- get
          put (y, x+y)
          ifib (n-1)
```

Example: We initialize the "variables" `x` and `y` with the value `1` for both before running `ifib`:

```hs
   *Main> runState (ifib 10) (1,1)
   (89,(89,144))
```

What we get is the result `89` *and* the final state `(89,144)`. For computing the Fibonacci function, we don't care what the final state is. We only want the result, which we obtain by the first projection.

```haskell
imperativefib :: (Eq a, Num a) => a -> a
imperativefib n = fst(runState (ifib n) (1 , 1))
```

## IO monad

This is predefined by the Haskell [runtime system](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts), and allows one to perform IO, providing several side-effecting operations, such as `putStrLn`, which prints to [stdout](https://en.wikipedia.org/wiki/Standard_streams).
```haskell
iofib :: (Show a, Eq a, Num a, Num b) => a -> IO b
iofib n = do
           putStrLn(show n) -- this is the only line changed
           if n == 0 || n == 1
           then return 1
           else do
                 x <- iofib(n-1)
                 y <- iofib(n-2)
                 return(x + y)
```
Example:
```
*Main> iofib 5
5
4
3
2
1
0
1
2
1
0
3
2
1
0
1
8

```
The last number is the result, and the other lines result from printing with `putStrLn`.

We will try to (begin to) understand this monad by considering an [Interaction monad]()

## More monads

The `Reader` monad allows one to consume from a list, with reading as the side-effect, while performing a computation.

### Random monad (self-learning)

To generate random numbers during computations. Find out about it yourself.

### Trees with labels at the leaves

This is similar to the list monad, and can be used to simulate non-determinism, where `Branch` is used whever we have a non-deterministic choice. Then we get the tree of all possible behaviours of the program.
```haskell
data T a = Leaf a | Branch (T a) (T a)

instance Functor T where
  -- fmap :: (a -> b) -> T a -> T b
  fmap f (Leaf a)     = Leaf(f a)
  fmap f (Branch t s) = Branch (fmap f t) (fmap f s)
```
This is like `concat :: [[a]] -> [a]`:
```haskell
tcollapse :: T(T a) -> T a
tcollapse (Leaf t)     = t
tcollapse (Branch t s) = Branch (tcollapse t) (tcollapse s)

instance Monad T where
  -- return :: a -> T a
  return = Leaf

  -- (>>=) :: T a -> (a -> T b) -> T b
  t >>= f = tcollapse(fmap f t)
```
Notice that we defined `Functor` and `Monad`, but not `Applicative`, which we now define using the boiler-plate code from the above recipe:
```haskell
instance Applicative T where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x
```
Notice also that there is an equivalent, direct definition of `>>=`:
```haskell
(>>>=) :: T a -> (a -> T b) -> T b
(Leaf x)     >>>= f = f x
(Branch t s) >>>= f = Branch (t >>>= f) (s >>>= f)
```
Notice also that, conversely, `tcollapse` is definable from this:
```haskell
tcollapse' ::  T(T a) -> T a
tcollapse' tt = tt >>>= (\t -> t >>>= return)
```
This is not the case for this monad only. For any monad you can define such a `collapse` function and then use it together with `fmap` to define `>>=`. Or, if you have `return` and `>>=`, you can use them to define `collapse` and `fmap`. (And this is a known fact in mathematics.)
