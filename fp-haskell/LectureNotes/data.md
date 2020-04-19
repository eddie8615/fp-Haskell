# User defined data types (and more)

You need to read Chapter 8 of the adopted textbook for full understanding (summarized in the [slides](../Resources/Book/slides/PDF/ch8.pdf)). And more chapters indicated in the discussion below.

## Converting these [markdown](https://en.wikipedia.org/wiki/Markdown) notes to a Haskell file automatically

This is done by the program [`mdtohs.hs`](../Resources/mdtohs.hs) included in the `Resources` directory, as follows:
```
$ cat data.md | ../Resources/runhaskell mdtohs.hs > data.hs
```
Hence any needed library imports should be mentioned here. We need the following for generating random inputs for testing:
```haskell
import System.Random
```

# Contents

* [Type synonyms](#typesynonyms)
* [User defined data types](#datatypes)
  * [The booleans](#booleans)
  * [Type isomorphisms](#typeisos)
  * [Weekdays](#weekdays)
* [Some important type constructors](#logic)
  * [The `Maybe` type constructor](#maybe)
  * [The `Either` type constructor](#either)
  * [The `And` type constructor, defined by ourselves](#and)
* [Lists](#lists)
  * [Implementing some basic operations on lists](#listops)
  * [An aside on accumulators](#accum)
  * [Higher-order functions](#hofns)
* [Binary trees](#bintrees)
  * [Basic functions on binary trees](#bintreefns)
  * [Directions, addresses and paths in binary trees](#bintreeaddr)
  * [Proofs on binary trees by induction](#bintreepf)
  * [Traversals in binary trees](#traversals)
  * [Inverting traversals (generating trees)](#gentree)
  * [Binary search trees](#bsts)
    * [Operations on binary search trees](#bstops)
    * [Testing and experimenting with the BST code](#bsttest)
    * [BST sort, quick sort and merge sort](#bstsort)
* [Other kinds of trees](#moretrees)
  * [Rose trees](#rosetrees)
  * [Game trees](#gametrees)
  * [Permutation trees, list permutations, and paths in such trees (hard)](#ptrees)
  * [Expression trees](#exprtrees)

* [Types with a single constructor](#newtype)

<a name="typesynonyms"></a>
# Type synonyms

Sometimes, mainly for the sake of clarity, we may wish to give a new name to an existing type.
For example, the Haskell prelude defines a string to be a list of characters:
```hs
type String = [Char]
```
Since `String` is just a type synonym, operations such as list concatenation and reverse
```hs
(++) :: [a] -> [a] -> [a]
reverse :: [a] -> [a]
```
can be freely applied to strings:
```hs
> "abc" ++ reverse "def"
"abcfed"
```
Type synonyms can also have parameters, as in e.g.
```haskell
type Lst a = [a]
```

<a name="datatypes"></a>
# User defined data types

<a name="booleans"></a>
## The booleans

The booleans are defined as follows in Haskell, in the prelude:
```hs
data Bool = False | True
```
This defines a new type, called `Bool`, with two elements (or *constructors*), called `False` and `True`:
```hs
   False :: Bool
   True  :: Bool
```
Functions over a data type can be conveniently defined by **pattern-matching** on its constructors.
For example, in the prelude, the conjunction operation
```hs
(&&) :: Bool -> Bool -> Bool
```
is defined as follows:
```hs
False && _     = False
True  && True  = True
True  && False = False
```
A slightly subtle aspect of the semantics of pattern-matching in Haskell is that:

1. the different pattern-matching clauses are tried in order from top to bottom, and
2. the input arguments of the function are only evaluated to the extent needed to check whether they match the current pattern.

A consequence of this semantics is that the above definition of conjunction implements [short-circuit evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation): if the first argument is `False`, then the function returns `False` without even evaluating the second argument.

In contrast, consider the following alternative definition of conjunction:
```haskell
conj :: Bool -> Bool -> Bool
conj False False = False
conj False True  = False
conj True  False = False
conj True  True  = True
```
This version does *not* implement short-circuit evaluation: the second argument will always be evaluated regardless of the value of the first.
We can observe the difference between these two versions of conjunction by running the following experiment in the GHC interpreter:
```hs
> False && undefined
False
> False `conj` undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:28:11 in interactive:Ghci5
```

<a name="typeisos"></a>
## Type isomorphisms

Let's introduce another data type `BW` defined by
```haskell
data BW = Black | White
```
This type is *isomorphic* to the type `Bool`, via the functions
```haskell
bw2bool :: BW -> Bool
bw2bool Black = False
bw2bool White = True

bool2bw :: Bool -> BW
bool2bw False = Black
bool2bw True  = White
```
That the pair of functions `(bw2bool,bool2bw)` is an isomorphism means that they are mutually inverse, in the sense that
```hs
   bw2bool(bool2bw b) = b
```
for all `b :: Bool`, and
```hs
   bool2bw(bw2bool c) = c
```
for all `c :: BW`.

Type isomorphisms should *not* be confused with type synonyms.
For example, if we try to directly use a value of type `BW` where a value of type `Bool` is expected, we get a type error:
```hs
> let test = Black && True

<interactive>:39:1: error:
    • Couldn't match expected type ‘Bool’ with actual type ‘BW’
    • In the first argument of ‘(&&)’, namely ‘Black’
      In the expression: Black && True
      In an equation for ‘it’: it = Black && True
```
On the other hand, if we wrap up the values using the explicit coercions `bw2bool` and `bool2bw`, then everything checks:
```hs
> let test = bool2bw (bw2bool Black && True)
```

Of course, the names `Black` and `White` are arbitrary, and there is another isomorphism between `BW` and `Bool` that swaps `Black` with `True` and `White` with `False` instead.
```haskell
bw2bool' :: BW -> Bool
bw2bool' Black = True
bw2bool' White = False

bool2bw' :: Bool -> BW
bool2bw' False = White
bool2bw' True  = Black
```
And both of the types `Bool` and `BW` are of course isomorphic (again in two different ways) to the type
```haskell
data Bit = Zero | One
```

> **Note:** The syntax rules of Haskell require that both type names (here `Bool`, `BW`, `Bit`) and constructor names (here `False`, `True`, `Black`, `White`, `Zero`, `One`) should start with a capital letter.

<a name="weekdays"></a>
## Week days

Another example of a data type is
```hs
data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
```

We can ask Haskell to do some jobs for free for us (there are alternative ways of doing them ourselves with our own sweat):

```haskell
data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
               deriving (Show, Read, Eq, Ord, Enum)
```
This automatically adds the type `WeekDay` to the type classes with these five names, which give functions
```hs
   show :: WeekDay -> String
   read :: String -> WeekDay
   (==) :: WeekDay -> WeekDay -> Bool
   (<), (>), (<=), (>=) :: WeekDay -> WeekDay -> Bool
   succ, pred :: WeekDay -> WeekDay
```
Look this up in our textbook. Notice that `show` is the counterpart of Java's `toString`, and `read` does the opposite.
Some examples are:
```hs
> show Tue
"Tue"
> read "Tue" :: WeekDay  -- (the type annotation tells Haskell to try to parse the string as a WeekDay)
Tue
> read "Dog" :: WeekDay
*** Exception: Prelude.read: no parse
> Mon == Tue
False
> Mon < Tue
True
> succ Mon
Tue
> pred Tue
Mon
> [Mon .. Fri]
[Mon,Tue,Wed,Thu,Fri]
```

Monday doesn't have a predecessor, and Sunday doesn't have a successor:

```
> pred Mon
*** Exception: pred{WeekDay}: tried to take `pred' of first tag in enumeration
CallStack (from HasCallStack):
  error, called at data.hs:20:47 in main:Main
> succ Sun
*** Exception: succ{WeekDay}: tried to take `succ' of last tag in enumeration
CallStack (from HasCallStack):
  error, called at data.hs:20:47 in main:Main
```

<a name="logic"></a>
# Some important type constructors

<a name="maybe"></a>
## The `Maybe` type constructor

Sometimes a function may not be able to give a result, in which case we would like it to say explicitly that it cannot. We use the `Maybe` type from the prelude for that purpose:
```hs
data Maybe a = Nothing | Just a
```
Here `a` is a type parameter, and we have
```hs
   Nothing :: Maybe a
   Just    :: a -> Maybe a
```
This means that the constructor `Just` is a function. For example:
```hs
   Just 17 :: Maybe Integer
```
In Java the `Maybe` type constructor is called `Optional`.

### Example: find the first position an element occurs in a list

Since the first position is undefined if the element doesn't occur in the list, in that case we answer `Nothing`:
```haskell
firstPosition :: Eq a => a -> [a] -> Maybe Integer
firstPosition x []     = Nothing
firstPosition x (y:ys)
           | x == y    = Just 0
           | otherwise = case firstPosition x ys of
                           Nothing -> Nothing
                           Just n  -> Just(n+1)
```
For example:
```hd
> firstPosition 'a' ['a'..'z']
Just 0
> firstPosition 'b' ['a'..'z']
Just 1
> firstPosition 'z' ['a'..'z']
Just 25
> firstPosition '!' ['a'..'z']
Nothing
```
which we summarize as
```hs
    firstPosition 'a' ['a'..'z'] = Just 0
    firstPosition 'b' ['a'..'z'] = Just 1
    firstPosition 'z' ['a'..'z'] = Just 25
    firstPosition '!' ['a'..'z'] = Nothing
```
You are required to use the book to find out what `case` is and how it works in general, but in this example it should be clear. You are also required to use the book to find out about conditional definitions using `|` to indicate *guards* for equations.

We will use the `Maybe` type constructor very often, because there are many occasions in which some inputs are *invalid*.

<a name="either"></a>
## The `Either` type constructor

It is defined in the prelude as follows:
```hs
data Either a b = Left a | Right b
```
Then we have
```hs
    Left  :: a -> Either a b
    Right :: b -> Either a b
```
For example:
```hs
    Left 17     :: Either Integer String
    Right "abd" :: Either Integer String
```
The idea is that the type `Either a b` is the *disjoint union* of the types `a` and `b`, where we tag the elements of `a` with `Left` and those of `b` with `Right` in the union type. An example of its use is given below.

<a name="and"></a>
## The `And` type constructor, defined by ourselves

This has an isomorphic version predefined in the language, as we shall see soon:
```haskell
data And a b = Both a b
```
This is a type constructor with two parameters, and with an element constructor `Both`, which is a function
```hs
   Both :: a -> b -> And a b
```

For example, assuming we have defined types `MainDish`, `Dessert`, `Drink`,
```haskell
data MainDish = Chicken | Pasta | Vegetarian
data Dessert = Cake | IceCream | Fruit
data Drink = Tea | Coffee | Beer
```
we can define:
```haskell
type SaverMenu = Either (And MainDish Dessert) (And MainDish Drink)
```
which can be equivalently written
```hs
type SaverMenu = Either (MainDish `And` Dessert) (MainDish `And` Drink)
```
(Choose which form of definition you like better. Haskell accepts both.)

So what is available in the saver menu is either a main dish and a dessert, or else a main dish and a drink. It should be intuitively clear that this is isomorphic to
```haskell
type SaverMenu' = And MainDish (Either Dessert Drink)
```
meaning that you can have a main dish and either dessert or a drink. This intuition is made precise by the isomorphism
```haskell
prime :: SaverMenu -> SaverMenu'
prime (Left (Both m d)) = Both m (Left  d)
prime (Right(Both m d)) = Both m (Right d)

unprime :: SaverMenu' -> SaverMenu
unprime (Both m (Left  d)) = Left (Both m d)
unprime (Both m (Right d)) = Right(Both m d)
```
So, as a software developer, you can choose either `SaverMenu` as your implementation, or else `SaverMenu'`. They are different, but essentially equivalent.

We actually don't need to define `And`, because an equivalent type constructor is already available in Haskell, namely the type of pairs. We have an isomorphism as follows:
```haskell
and2pair :: And a b -> (a,b)
and2pair (Both x y) = (x,y)

pair2and :: (a,b) -> And a b
pair2and (x,y) = Both x y
```
And so we have further isomorphic versions of the saver menu type:
```haskell
type SaverMenu''  = Either (MainDish, Dessert) (MainDish, Drink)
type SaverMenu''' = (MainDish, Either Dessert Drink)
```
Lookup the type of pairs (tuple types) in the book and read about it.

<a name="lists"></a>
# Lists

With a pinch of salt, the type of lists is predefined by
```hs
data [a] = [] | a : [a]  -- not quite a Haskell definition
```
which says that a list of `a`'s is either empty, or else an element of the type `a` followed (indicated by `:`) by a list of `a`'s.
This is an example of a *recursive* data type definition.

Although the above is semantically correct, it is syntactically wrong, because Haskell (unfortunately) doesn't accept this kind of syntactical definition.
If we don't care about syntax, we can define an isomorphic version as follows:
```haskell
data List a = Nil | Cons a (List a)
```
Then the types of the constructors are
```hs
   Nil  :: List a
   Cons :: a -> List a -> List a
```
For example, the native list `[1,2,3]` is written `Cons 1 (Cons 2 (Cons 3 Nil))` in our isomorphic version of the type of lists. Let's define the isomorphism to make this clear:
```haskell
nativelist2ourlist :: [a] -> List a
nativelist2ourlist []     = Nil
nativelist2ourlist (x:xs) = Cons x (nativelist2ourlist xs)

ourlist2nativelist :: List a -> [a]
ourlist2nativelist Nil = []
ourlist2nativelist (Cons x xs) = x:ourlist2nativelist xs
```
Observe that these coercions are defined recursively.

<a name="listops"></a>
## Implementing some basic operations on lists

Let's write our own versions of the list concatenation ("append") and reverse operations from the prelude:

```haskell
append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

rev :: List a -> List a
rev Nil         = Nil
rev (Cons x xs) = rev xs `append` (Cons x Nil)
```

We can try to test that these do the right thing by comparing them to the implementations of list concatenation and reversal in the Haskell prelude, using the isomorphism between `List a` and `[a]`.
Indeed, we expect that
```hs
ourlist2nativelist (append (nativelist2ourlist xs) (nativelist2ourlist ys)) == xs ++ ys
```
and
```hs
ourlist2nativelist (rev (nativelist2ourlist xs)) == reverse xs
```
should hold for all native lists `xs, ys :: [a]`.
Let's test these properties:
```hs
> let xs = [1..5]
> let ys = [6..10]
> ourlist2nativelist (append (nativelist2ourlist xs) (nativelist2ourlist ys)) == xs ++ ys
True
> ourlist2nativelist (rev (nativelist2ourlist xs)) == reverse xs
True
```
Of course, here we have only tested on a couple examples, but it is true in general. (Question: how would you *prove* this?)

Although our definitions are functionally correct, there is a more subtle problem with our implementation of `rev`.
By inspection of the code, `append xs ys` computes the concatenation of two lists in time O(n), where n is the length of `xs`, since each recursive call to `append` decreases the length of `xs` by one, and the calls to `Cons` are constant time.
On the other hand, `rev` is O(n²) by the same argument, since each recursive call to `rev` decreases the length of `xs` by one, and each call to `append` is O(n).

This is not just a theoretical problem &mdash; we quickly bump into it if we compare reversing a reasonably large list using the native `reverse` function versus the implementation `rev` above.
```hs
> let xs = [1..10^5]
> length (reverse xs)  -- this is fast (we return the length of the reversed list in order to keep the output small)
100000
> length (ourlist2nativelist (rev (nativelist2ourlist xs)))  -- this is really slow, so we give up and hit Control-C
  C-c C-cInterrupted.
```
There's a much more efficient way of implementing reversal by introducing a helper function with an extra argument:
```haskell
fastrev :: List a -> List a
fastrev xs = revapp xs Nil
  where
    revapp :: List a -> List a -> List a
    revapp (Cons x xs) ys = revapp xs (Cons x ys)
    revapp Nil         ys = ys
```
One way to think of the second argument of the helper function `revapp` is as a stack, initially set to be empty (`Nil`).
The function recursively scans the input from the first argument, pushing each element onto the stack (second argument).
When there are no more input elements, the stack is simply popped directly to the output, with all of the elements of the original list now in reverse order.

Here's a concrete illustration of how this works, unrolling the definitions of `fastrev` and `revapp` to reverse a four-element list:

```hs
  fastrev (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
= revapp (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) Nil
= revapp (Cons 2 (Cons 3 (Cons 4 Nil))) (Cons 1 Nil)
= revapp (Cons 3 (Cons 4 Nil)) (Cons 2 (Cons 1 Nil))
= revapp (Cons 4 Nil) (Cons 3 (Cons 2 (Cons 1 Nil)))
= revapp Nil (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))
= Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil)))
```

Another way of thinking of the function `revapp` is suggested by its name: given two lists `xs` and `ys`, `revapp xs ys` computes the reversal of `xs` *appended* with `ys`.
It's not hard to see that this binary operation `revapp` is *more general* than the original unary reversal operation: the latter can be recovered by taking `ys = Nil`.
On the other hand, `revapp` is much more efficient than our original function `rev`, being only O(n) in the length of its first argument `xs`.

This pattern &mdash; where we manage to solve a problem or solve it more efficiently by replacing it with a more general and seemingly more difficult problem &mdash; happens again and again in functional programming.

<a name="accum"></a>
## An aside on accumulators

The extra argument `ys` that we used in the helper function `revapp` is sometimes called an "accumulator", since it accumulates a value that is eventually passed to the output.
Above we saw how an accumulator could be used to turn an O(n²) algorithm into an O(n) algorithm for list reversal.
For an even starker example, consider the problem of computing the [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number) Fₙ.

The mathematical definition of the Fibonacci sequence in Wikipedia may be translated directly into the following Haskell code:

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

But although this definition is correct, it is extremely inefficient!

We can already see this if we try to use the above definition to compute, say, the first 32 Fibonacci numbers:

```hs
> :set +s -- ask ghci to print time and space usage
> [fib n | n <- [0..31]]
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269]
(10.23 secs, 4,086,282,024 bytes)
```

Over ten seconds to compute 32 Fibonacci numbers!
Indeed, the running time of `fib n` is roughly O(2ⁿ), since the recursive case makes two calls to `fib` while only decreasing `n` by 1 or 2.

Here is an alternative, much more efficient implementation using a pair of accumulators `x` and `y`:

```haskell
fastfib n = fibAcc n 0 1
  where
    fibAcc 0 x y = x
    fibAcc 1 x y = y
    fibAcc n x y = fibAcc (n-1) y (x+y)
```

With this implementation we have no trouble computing, say, the first 100 Fibonacci numbers in a fraction of a second:

```hs
> [fastfib n | n <- [0..99]]
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,32951280099,53316291173,86267571272,139583862445,225851433717,365435296162,591286729879,956722026041,1548008755920,2504730781961,4052739537881,6557470319842,10610209857723,17167680177565,27777890035288,44945570212853,72723460248141,117669030460994,190392490709135,308061521170129,498454011879264,806515533049393,1304969544928657,2111485077978050,3416454622906707,5527939700884757,8944394323791464,14472334024676221,23416728348467685,37889062373143906,61305790721611591,99194853094755497,160500643816367088,259695496911122585,420196140727489673,679891637638612258,1100087778366101931,1779979416004714189,2880067194370816120,4660046610375530309,7540113804746346429,12200160415121876738,19740274219868223167,31940434634990099905,51680708854858323072,83621143489848422977,135301852344706746049,218922995834555169026]
(0.02 secs, 3,057,552 bytes)
```

Again to see what is going on it is helpful to unroll the definitions on a concrete example:

```hs
  fastfib 7
= fibAcc 7 0 1
= fibAcc 6 1 1
= fibAcc 5 1 2
= fibAcc 4 2 3
= fibAcc 3 3 5
= fibAcc 2 5 8
= fibAcc 1 8 13
= 13
```

We can see that this functional implementation of the Fibonacci numbers using a pair of accumulator arguments `x` and `y` works very similarly to the way one might compute the Fibonacci numbers in Java, by updating a pair of variables `x` and `y` inside a loop:

```java
static int fastfib(int n) {
  int x = 0, y = 1;
  while (n > 1) {
     int z = x+y;
     x = y;
     y = z;
     n = n-1;
  }
  return (n == 0 ? x : y);
}
```

In addition to this low-level view of what `fastfib` and in turn `fibAcc` is doing, there is also a more high-level view (as we saw before in the case of `revapp`).
Can you identify a sense in which the helper function `fibAcc n x y` computes a *more general* function than `fib n`?

<a name="hofns"></a>
## Higher-order functions

Read more about these functions in Chapter 7 of the book.

### map

```hs
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

Example:
```hs
> map (*2) [1..5]
[2,4,6,8,10]
> map show [1..5]
["1","2","3","4","5"]
```

### filter

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter pred []     = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
```

Example:
```hs
> filter (\x -> x `mod` 2 == 0) [1..5]
[2,4]
```

### foldl and foldr

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs
```

Example:
```hs
  foldr (*) 1 [1..5]
= 1 * foldr (*) 1 [2..5]
= 1 * (2 * foldr (*) 1 [3..5])
= 1 * (2 * (3 * foldr (*) 1 [4..5]))
= 1 * (2 * (3 * (4 * foldr (*) 1 [5])))
= 1 * (2 * (3 * (4 * (5 * foldr (*) 1 []))))
= 1 * (2 * (3 * (4 * (5 * 1))))
= 120
```

```hs
  foldl (+) 0 [1..5]
= foldl (+) (0 + 1) [2..5]
= foldl (+) ((0 + 1) + 2) [3..5]
= foldl (+) (((0 + 1) + 2) + 3) [4..5]
= foldl (+) ((((0 + 1) + 2) + 3) + 4) [5]
= foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) []
= ((((0 + 1) + 2) + 3) + 4) + 5
= 15
```

Exercise: implement list concatenation using `foldr`

Exercise: implement list reversal using `foldl`

<a name="bintrees"></a>
# Binary trees

A binary tree over elements of a type `a` is either empty, or consists of a root labelled by an element of the type `a` followed by two binary trees, called the left and right subtrees:
```hs
data BT a = Empty | Fork a (BT a) (BT a)
```
* We have the empty tree, to be called `Empty`.
  We take the convention that empty trees are drawn as dangling leaves.
  ```
                              |
  ```


* Given two trees `l` and `r` and an element `x::a`, we have the new tree
  ```
                              x
                             / \
                            /   \
                           l     r
  ```
  written `Fork x l r`.

For example, the tree
```
                             8
                            / \
                           /   \
                          4     16
                         / \   / \
                        2        20
                       / \      /  \
```
is written, in this notation, as
```haskell
btexample = Fork 8 (Fork 4 (Fork 2 Empty Empty) Empty) (Fork 16 Empty (Fork 20 Empty Empty))
```

We can ask Haskell to do some work for us by deriving things as above.

```haskell
data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)
```

<a name="bintreefns"></a>
## Basic functions on binary trees

To get started, let's mirror trees, so that e.g. from the above we get
```
                             8
                            / \
                           /   \
                          16    4
                         / \   / \
                        20        2
                       / \       / \
```
We do this as follows:
```haskell
mirror :: BT a -> BT a
mirror Empty = Empty
mirror (Fork x l r) = Fork x (mirror r) (mirror l)
```
Running this on the above example we get
```hs
    mirror btexample = Fork 8 (Fork 16 (Fork 20 Empty Empty) Empty) (Fork 4 Empty (Fork 2 Empty Empty))
```
This notation for trees is not very good for visualizing them, as you
can see, but is very good for computation.

We define the *size* of a tree as its total number of nodes:
```haskell
size :: BT a -> Integer
size Empty        = 0
size (Fork x l r) = 1 + size l + size r
```
Since we are considering binary trees, the size (i.e., the number of nodes) is also equal to the number of leaves minus one:
```haskell
leaves :: BT a -> Integer
leaves Empty        = 1
leaves (Fork x l r) = leaves l + leaves r
```
We define the *height* of a tree to be the length of the longest path from the root, measured in number of nodes:
```haskell
height :: BT a -> Integer
height Empty        = 0
height (Fork x l r) =  1 + max (height l) (height r)
```
A balanced binary tree has height approximately log of its size, whereas a binary tree which is very unbalanced, such as
```
                            20
                           / \
                          16
                         / \
                        8
                       / \
                      4
                     / \
                    2
                   / \
```
```haskell
btleft = Fork 20 (Fork 16 (Fork 8 (Fork 4 (Fork 2 Empty Empty) Empty) Empty) Empty) Empty
```
has height approximately equal to its size.

<a name="bintreeaddr"></a>
## Directions, addresses and paths in binary trees

To pick a subtree of a binary tree, we go left or right successively, until
we find it. But a wrong list of directions, called an address here, may be given, and hence we need the `Maybe` type for the output:
```haskell
data Direction = L | R deriving (Show)
type Address   = [Direction]

subtree :: Address -> BT a -> Maybe(BT a)
subtree []     t            = Just t
subtree (_:_)  Empty        = Nothing
subtree (L:ds) (Fork _ l _) = subtree ds l
subtree (R:ds) (Fork _ _ r) = subtree ds r
```
Following the above pattern, we can define a function that checks whether an address in a given tree is valid:
```haskell
isValid :: Address -> BT a -> Bool
isValid []     _            = True
isValid (_:_)  Empty        = False
isValid (L:ds) (Fork _ l _) = isValid ds l
isValid (R:ds) (Fork _ _ r) = isValid ds r
```
The list of valid addresses for subtrees can be computed as follows:
```haskell
validAddresses :: BT a -> [Address]
validAddresses Empty        = [[]]
validAddresses (Fork _ l r) = [[]]
                           ++ [L:ds | ds <- validAddresses l]
                           ++ [R:ds | ds <- validAddresses r]
```
List comprehensions can always be eliminated. In this example they become
```haskell
validAddresses' :: BT a -> [Address]
validAddresses' Empty        = [[]]
validAddresses' (Fork _ l r) = [[]]
                            ++ (map (L:) (validAddresses' l))
                            ++ (map (R:) (validAddresses' r))
```


We expect that
```hs
    isValid ds t = ds `elem` (validAddresses t)
```
Or, in words, an address is valid if and only if it is an element of the list of valid addresses. Should this be intuitively clear? The statement, yes. But the fact, given our definitions, I don't think so. I would say that it requires a convincing argument. In any case, intuition is something we develop based on convincing arguments we learn.

The list of all paths from the root to a leaf has a similar definition:
```haskell
btpaths :: BT a -> [[a]]
btpaths Empty        = [[]]
btpaths (Fork x l r) = [x:xs | xs <- btpaths l]
                    ++ [x:xs | xs <- btpaths r]
```

<a name="bintreepf"></a>
## Proofs on binary trees by induction

If we have a property `P` of trees, and we want to show that
`P(t)` holds for all trees `t`, we can do this by *induction on trees* as follows:
* Argue that `P(Empty)` holds.
* Argue that if `P(l)` and `P(r)` hold for given trees `l` and `r`, then it holds for `P(Fork x l r)` where `x` is arbitrary.

We are not going to emphasize proofs in this module, but we will indicate when some claims genuinely require proofs, and, moreover, we will try to be precise regarding the specifications of the programs we write.

### Functional proofs

The dependently typed language [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) allows to write functional programs *and* their correctness proofs, where the [proofs themselves are written as functional programs](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence).
As an example, [here is a computer-checked proof](http://www.cs.bham.ac.uk/~mhe/fp-learning-2017-2018/html/Agda-in-a-Hurry.html) of the above relation between the functions `isValid` and `validAddresses` in Agda.
This is not examinable, and is included here for the sake of illustration only.

<a name="traversals"></a>
## Traversals in binary trees

We now define the standard in-order and pre-order [traversals](https://en.wikipedia.org/wiki/Tree_traversal).
```haskell
treeInOrder :: BT a -> [a]
treeInOrder Empty = []
treeInOrder (Fork x l r) = treeInOrder l ++ [x] ++ treeInOrder r

treePreOrder :: BT a -> [a]
treePreOrder Empty = []
treePreOrder (Fork x l r) = [x] ++ treePreOrder l ++ treePreOrder r
```
For instance, for the trees `btexample` and `btleft` considered above,
```
                             8
                            / \
     btexample =           /   \
                          4     16
                         / \   / \
                        2        20
                       / \      /  \
```
```
                            20
                           / \
                          16
                         / \
        btleft =        8
                       / \
                      4
                     / \
                    2
                   / \
```
we get:
```hs
> (treeInOrder btexample, treePreOrder btexample)
([2,4,8,16,20],[8,4,2,16,20])
> (treeInOrder btleft, treePreOrder btleft)
([2,4,8,16,20],[20,16,8,4,2])
```

[Breadth-first traversal](https://en.wikipedia.org/wiki/Breadth-first_search) is trickier. We first define a function that given a tree, produces a lists of lists, with the nodes of level zero (just the root), then the nodes of level one (the successors of the root), then the nodes of level two, and so on:
```haskell
levels :: BT a -> [[a]]
levels Empty        = []
levels (Fork x l r) = [[x]] ++ zipappend (levels l) (levels r)
  where
    zipappend []       yss      = yss
    zipappend xss      []       = xss
    zipappend (xs:xss) (ys:yss) = (xs ++ ys) : zipappend xss yss
```
(Compare `zipappend` to the prelude function [zipWith](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#zipWith).)
For example:
```hs
> levels btexample
[[8],[4,16],[2,20]]
> levels btleft
[[20],[16],[8],[4],[2]]
```
With this we can define
```haskell
treeBreadthFirst :: BT a -> [a]
treeBreadthFirst = concat . levels
```
where `.` stands for function composition (look it up in our textbook), and the prelude function `concat :: [[a]] -> [a]` concatenates a list of lists, for example getting `[8,4,16,2,20]` from `[[8],[4,16],[2,20]]`. For further discussion about breadth-first search, see [The under-appreciated unfold](https://dl.acm.org/citation.cfm?doid=289423.289455) (a free version is at the [authors' web page](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/unfold.ps.gz)), but this is probably beyond your current level for most of you.

<!--
Here are some more operations on trees. Given an element, find all the addresses it occurs at a given tree, which may result in an empty list (if it doesn't occur in the tree), or in a list with many addresses (if it occurs many times in the tree).
```haskell
positions :: Eq a => a -> BT a -> [Address]
positions x Empty        = []
positions x (Fork y l r) = [[] | x == y]
                        ++ [L:ds | ds <- positions x l]
                        ++ [R:ds | ds <- positions x r]
```
Find an element at a valid position (similar to `subtree`):
```haskell
elementAt :: Address -> BT a -> a
elementAt []     Empty       = undefined
elementAt []    (Fork x _ _) = x
elementAt (d:ds) Empty       = undefined
elementAt (L:ds)(Fork _ l _) = elementAt ds l
elementAt (R:ds)(Fork _ _ r) = elementAt ds r
```
We have that if `ds` is a member of the list `positions x t`, then `elementAt ds t = x`.

We also consider binary *search* trees below, but before that we pause to consider expression trees.
-->

<a name="gentree"></a>
## Inverting traversals (generating trees)

Many different trees can have the same (in-order/pre-order/breadth-first) traversal, as we saw above with `btexample` and `btleft`, which have the same in-order traversal.
In other words, all of the functions
```hs
treeInOrder, treePreOrder, treeBreadthFirst :: BT a -> [a]
```
are *non-injective* and hence non-invertible.
Nonetheless, an interesting and tractable problem is to try to construct a binary tree with a given (in-order/pre-order/breadth-first) traversal, or even to generate *all possible binary trees* with a given traversal.

As an example, the following will produce a *balanced* binary tree given its in-order traversal (which will be a binary *search* tree if the input is sorted):
```haskell
balancedTree :: [a] -> BT a
balancedTree [] = Empty
balancedTree xs = let (ys, x:zs) = splitAt (length xs `div` 2) xs in
                  Fork x (balancedTree ys) (balancedTree zs)
```
(The prelude function [`splitAt`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:splitAt) splits a list in two lists at a given position.)
This satisfies the equation
```hs
    treeInOrder (balancedTree xs) == xs
```
for all `xs :: [a]`.
In the other direction, it is certainly **not** the case that
```hs
    balancedTree (treeInOrder t) == t
```
for all `t :: Bin a`, for instance
```hs
balancedTree (treeInOrder btleft) == Fork 8 (Fork 4 (Fork 2 Empty Empty) Empty) (Fork 20 (Fork 16 Empty Empty) Empty)
```
which is not equal to `btleft`.
Indeed, the composite function
```haskell
balance :: BT a -> BT a
balance = balancedTree . treeInOrder
```
which applies `treeInOrder` to a tree followed by `balancedTree` to the resulting list can be seen as an operation for rebalancing a binary tree.

Now, using list comprehensions, it is a small step from the function `balancedTree` above to a function generating *all* binary trees with a given in-order traversal.
```haskell
inOrderTree :: [a] -> [BT a]
inOrderTree [] = [Empty]
inOrderTree xs = [Fork x l r | i <- [0..length xs-1],
                               let (ys, x:zs) = splitAt i xs,
                               l <- inOrderTree ys, r <- inOrderTree zs]
```
This satisfies the property that
```hs
elem t (inOrderTree xs)
```
if and only if
```hs
treeInOrder t == xs
```
for all `t :: Bin a` and `xs :: [a]`.
For example, running
```hs
> inOrderTree [1..3]
[Fork 1 Empty (Fork 2 Empty (Fork 3 Empty Empty)),Fork 1 Empty (Fork 3 (Fork 2 Empty Empty) Empty),Fork 2 (Fork 1 Empty Empty) (Fork 3 Empty Empty),Fork 3 (Fork 1 Empty (Fork 2 Empty Empty)) Empty,Fork 3 (Fork 2 (Fork 1 Empty Empty) Empty) Empty]
```
successfully computes all five binary search trees whose in-order traversal is `[1,2,3]`:
```
   1
  / \
     2
    / \
       3
      / \
Fork 1 Empty (Fork 2 Empty (Fork 3 Empty Empty))

   1
  / \
     3
    / \
   2
  / \
Fork 1 Empty (Fork 3 (Fork 2 Empty Empty) Empty)

     2
    / \
   /   \
  1     3
 / \   / \
Fork 2 (Fork 1 Empty Empty) (Fork 3 Empty Empty)

    3
   / \
  1
 / \
    2
   / \
Fork 3 (Fork 1 Empty (Fork 2 Empty Empty)) Empty

      3
     / \
    2
   / \
  1
 / \
Fork 3 (Fork 2 (Fork 1 Empty Empty) Empty) Empty
```

**Exercise:** write a function `preOrderTree :: [a] -> [BT a]`, with the property that `elem t (preOrderTree xs)` if and only if `treePreOrder t == xs` for all `t :: Bin a` and `xs :: [a]`.

**Very hard exercise:** write a function `breadthFirstTree :: [a] -> [BT a]`, with the property that `elem t (breadthFirstTree xs)` if and only if `treeBreadthFirst t == xs` for all `t :: Bin a` and `xs :: [a]`. ([Hint](https://patternsinfp.wordpress.com/2015/03/05/breadth-first-traversal/))

<a name="bsts"></a>
## Binary search trees

Now let's consider [binary *search* trees](https://en.wikipedia.org/wiki/Binary_search_tree).

<a name="bstops"></a>
### Operations on binary search trees

The following checks whether a binary tree (containing values of some *ordered* type) is a binary search tree:
```haskell
isBST :: Ord a => BT a -> Bool
isBST Empty        = True
isBST (Fork x l r) = allSmaller x l
                  && allBigger  x r
                  && isBST l
                  && isBST r

allSmaller :: Ord a => a -> BT a -> Bool
allSmaller x Empty        = True
allSmaller x (Fork y l r) = y < x
                         && allSmaller x l
                         && allSmaller x r

allBigger :: Ord a => a -> BT a -> Bool
allBigger x Empty = True
allBigger x (Fork y l r) = y > x
                         && allBigger x l
                         && allBigger x r
```
This is not an efficient way of checking the binary search tree
property (it runs in quadratic time), but is very close to the English-prose definition you know.

We get
```hs
   isBST btexample = True
   isBST btleft = True
   isBST (mirror btexample) = False
```
A better way of checking the BST property (in linear time) is to produce its in-order traversal. It turns out that a tree has the BST property if and only if
its in-order traversal is a sorted list (one can prove this by induction on trees).
```haskell
isBST' :: Ord a => BT a -> Bool
isBST' t = isIncreasing(treeInOrder t)

isIncreasing :: Ord a => [a] -> Bool
isIncreasing []       = True
isIncreasing (x:[])   = True
isIncreasing (x:y:zs) = x < y && isIncreasing(y:zs)
```
**Puzzle:** Can you write another version of isBST that runs in linear time *without* producing the in-order traversal list as an intermediate step?

As you will remember, the point of binary search trees is that they can be
searched fast (logarithmic time) if they are sufficiently balanced:
```haskell
occurs :: Ord a => a -> BT a -> Bool
occurs x Empty        = False
occurs x (Fork y l r) = x == y
                     || (x < y && occurs x l)
                     || (x > y && occurs x r)
```
<!--
What makes this fast is that `||` and `&&` use [short-circuit evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation).
If the first condition is true, the second one is not evaluated. Similarly, for `&&`, if the first condition is false then the second one is not evaluated.
-->

For the next function, we want that if the input is a BST, then so is
the output.
```haskell
insert :: Ord a => a -> BT a -> BT a
insert x Empty                 = Fork x Empty Empty
insert x (Fork y l r) | x == y = Fork y l r
                      | x <  y = Fork y (insert x l) r
                      | x >  y = Fork y l (insert x r)
```
If the element to be inserted is already in the tree, we give the
same tree as a result, as our BST's are not allowed to have repetitions (according to the definition of isBST). Of course, there are alternative ways, such as using a `Maybe` return type to indicate with `Nothing` that the element is already there:
```haskell
insert' :: Ord a => a -> BT a -> Maybe(BT a)
insert' x Empty                = Just(Fork x Empty Empty)
insert' x (Fork y l r)| x == y = Nothing
                      | x <  y = case insert' x l of
                                   Nothing -> Nothing
                                   Just l' -> Just(Fork y l' r)
                      | x >  y = case insert' x r of
                                   Nothing -> Nothing
                                   Just r' -> Just(Fork y l r')
```
This code can be slightly simplified using the fact that `Maybe` is a
monad, but we'll come to that later.

The most difficult function for BST's is deletion:
```haskell
delete :: Ord a => a -> BT a -> BT a
delete x Empty = Empty -- or you may prefer undefined (and even Nothing)
delete x (Fork y l r) | x < y                = Fork y (delete x l) r
                      | x > y                = Fork y l (delete x r)
                      | x == y && l == Empty = r
                      | x == y && r == Empty = l
                      | otherwise            = Fork (largestOf l) (withoutLargest l) r

largestOf :: Ord a => BT a -> a
largestOf Empty            = undefined
largestOf (Fork x l Empty) = x
largestOf (Fork x l r)     = largestOf r

withoutLargest :: Ord a => BT a -> BT a
withoutLargest Empty            = undefined
withoutLargest (Fork x l Empty) = l
withoutLargest (Fork x l r)     = Fork x l (withoutLargest r)
```
* Can you write a function `delete'` with a `Maybe` return type to
indicate that there is nothing to delete?

* Can you combine the last two functions `largestOf` and
`withoutLargest` into a single one, using a pair type as the
result, so as to get a more efficient version of the delete
function? And then this together with `Maybe` rather than using `undefined`?

<a name="bsttest"></a>
### Testing and experimenting with the BST code

It is very important for you to learn how to [test](https://en.wikipedia.org/wiki/Software_testing) your code (for correctness) and experiment with it (for efficiency), both for this module and for your professional life as a software developer, if this is your chosen career path after you finish your degree. Here are some starting ideas for you to design your own tests for your work for this module. The main one is that you write code to test that what should be true is indeed true, and to test the run times.

Later we will meet the random monad. For the moment we work with an
infinite list of [pseudo random](https://en.wikipedia.org/wiki/Pseudorandomness)  integers (needs `import
System.Random` at the top of a Haskell file):
```haskell
randomInts :: [Int]
randomInts = randomRs (minBound,maxBound) (mkStdGen seed)
             where seed = 42
```
We can insert many elements in a tree as follows:
```haskell
inserts :: Ord a => [a] -> BT a -> BT a
inserts []     t = t
inserts (x:xs) t = inserts xs (insert x t)
```
Then we define data for our testing and experimentation:
```haskell
aBigBST :: BT Int
aBigBST = inserts (take (10^6) randomInts) Empty

itsHeight = height aBigBST
itsSize   = size aBigBST
itsBST    = isBST aBigBST
itsBST'   = isBST' aBigBST
```
Now we test and experiment with this as follows in `ghci` (where in practice we should have created a file with code for this):
```hs
*Main> :set +s -- ask ghci to print time and space usage
*Main> itsHeight
49
(20.66 secs, 11,618,092,240 bytes)
*Main> itsHeight      -- again
49
(0.01 secs, 0 bytes)  -- fast because it got stored (part of what laziness is)
*Main> itsSize
1000000
(0.50 secs, 248,066,824 bytes) -- fast because the tree is already computed
*Main> itsBST
True
(12.84 secs, 8,691,110,224 bytes) -- slow because of inefficient algorithm
*Main> itsBST'
True
(1.10 secs, 1,198,200,632 bytes) -- the alternative algorithm is much more efficient
*Main>
```
Notice the height is not optimal (that would be `log2(10^6)`,
which is approximately 30), but it is low (namely 49), meaning that searches in
that three will take at most 49 steps, and hence will be really fast,
even when what we are looking for is not in the tree:
```
*Main> occurs 17 aBigBST
False
(0.01 secs, 0 bytes)
```
Delete many elements from a tree:
```haskell
deletes :: Ord a => [a] -> BT a -> BT a
deletes []     t = t
deletes (x:xs) t = deletes xs (delete x t)
```
Delete half of the elements we inserted:
```haskell
aSmallerTree :: BT Int
aSmallerTree = deletes (take (5 * (10^5)) randomInts) aBigBST
```
This tree is not computed until it is used, because of lazy
evaluation. The following forces all deletions to be performed.
```hs
*Main> height aSmallerTree
45
(8.88 secs, 5,515,246,520 bytes)
```
Then, as a sanity check, we test whether our deletion algorithm didn't
destroy the BST property:
```hs
*Main> isBST' aSmallerTree
True
(0.52 secs, 583,510,072 bytes)
```
Good.
```haskell
evenBigger :: BT Int
evenBigger = inserts (take (10^7) randomInts) Empty
```
Oops:
```hs
*Main> height evenBigger
*** Exception: stack overflow
```
There is an option to increase the size of the stack when you run ghci:
```hs
$  ghci data.hs +RTS -K1G -RTS
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( data.hs, interpreted )
Ok, modules loaded: Main.
*Main> :set +s
*Main> height evenBigger
58
(269.24 secs, 134,999,934,888 bytes)
*Main> 269.24 / 60
4.487333333333334
*Main>
```
That's 4.49 minutes on my machine. But, once the tree is computed the first time, it stays
computed, and we get e.g.
```hs
*Main> height evenBigger
58
(4.24 secs, 2,506,734,080 bytes)
*Main> occurs 17 evenBigger
False
(0.01 secs, 92,952 bytes) -- magic? :-)
```
The last expression would take a long time if executed immediately
after loading the file, before the tree is computed:
```hs
*Main> isBST' evenBigger
True
(15.38 secs, 13,271,069,712 bytes)
```
So we have a BST with 10,000,000 elements, of height 58, which can hence be searched fast. We can easily get bigger trees. Now consider
```haskell
fullBST :: Integer -> Integer -> BT Integer
fullBST x y | x == y    = Fork x Empty Empty
            | x+1 == y  = Fork y (Fork x Empty Empty) Empty
            | x+1 <  y  = Fork m (fullBST x (m-1)) (fullBST (m+1) y)
            | otherwise = undefined
  where m = (x + y) `div` 2
```
What does this function do? Convince yourself that if `x<=y` then `treeInOrder(fullBST x y) = [x..y]`. For instance
```hs
    treeInOrder(fullBST 2 11) = [2,3,4,5,6,7,8,9,10,11]
```
In the following, the same tree gets computed (to the extent that it
is needed) every time because it is not assigned to a variable:
```hs
*Main> occurs 17 (fullBST 1 (10^8))
True
(0.01 secs, 0 bytes)
*Main> occurs 17 (delete 17 (fullBST 1 (10^8)))
False
(0.01 secs, 0 bytes)
*Main> height (fullBST 1 (10^8))
27
(123.90 secs, 63,316,811,920 bytes)
*Main> 123.90 / 60
2.065
```
This amounts to two minutes. To figure out that 17 is (not) in the above trees, it is not necessary to build the whole tree (Haskell is lazy), and this is why this is faster than computing the height.

Finally:
```hs
*Main> deletes (take (10^5) randomInts) (inserts (take (10^5) randomInts) Empty)
Empty
(2.74 secs, 1,686,246,488 bytes)
*Main> deletes (take (10^6) randomInts) (inserts (take (10^6) randomInts) Empty)
Empty
(42.25 secs, 20,813,980,136 bytes)
```

<a name="bstsort"></a>
### BST sort, quick sort and merge sort

Of course, as you know, you can sort lists using this, but it will
remove repetitions because binary search trees don't allow for repetitions (accounted for by our version of `insert` and `inserts`):
```haskell
bstsort :: Ord a => [a] -> [a]
bstsort xs = treeInOrder(inserts xs Empty)
```
A form of quick sort is easy to write:
```haskell
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [l | l <- xs, l < x]
            ++ [x]
            ++ qsort [r | r <- xs, r >= x]
```
You can easily modify this to remove duplicates in the resulting sorted list (try it). Merge sort can be
defined as follows, where we split a list in even and odd positions, rather than first half and second half, for the sake of simplicity and efficiency:
```haskell
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

eosplit :: [a] -> ([a],[a])
eosplit []       = ([],[])
eosplit [x]      = ([x],[])
eosplit (e:o:xs) = case eosplit xs of
                     (es,os) -> (e:es, o:os)

msort :: Ord a => [a] -> [a]
msort xs | length xs <= 1  =  xs
         | otherwise       = merge (msort es) (msort os)
                             where (es, os) = eosplit xs
```

As you know, quick sort for sorted or reverse-sorted lists is slow (quadratic time).
So don't bother to try e.g.
```hs
    sum(qsort [1..(10^5)])
```
as it would take `(10^5)^2` steps, which amounts to `10^10`, or 10 billion, steps.
The same will happen with `bstsort`. However, for random lists they work
better, with `qsort` faster than `bstsort`. Recall that `msort` is always  `n * log n`. We haven't tested `msort` for the sake of brevity (these notes are already long as they are), but you may wish to do this yourself.
```haskell
bigList  = take (10^5) randomInts
hugeList = take (10^6) randomInts
```
We test this as follows
```hs
*Main> length bigList
100000
(0.02 secs, 4,522,952 bytes)
*Main> length(qsort bigList )
100000
(1.71 secs, 465,819,512 bytes)
*Main> length(bstsort bigList )
100000
(2.45 secs, 1,013,907,712 bytes)
*Main> length hugeList
1000000
(0.02 secs, 0 bytes)
*Main> length (qsort hugeList)
1000000
(21.94 secs, 7,578,153,768 bytes)
*Main> length (bstsort hugeList)
1000000
(37.34 secs, 12,514,195,728 bytes)
*Main> qsort hugeList == bstsort hugeList
True
(58.45 secs, 18,272,526,856 bytes)
*Main>
```
You may wish to perform your own tests and experiments with merge sort.

<a name="moretrees"></a>
# Other kinds of trees

<a name="rosetrees"></a>
## Rose trees

Instead of having zero-branching (as `Empty`) and binary branching (as `Fork`) as in binary trees,
we can branch any number of times, including zero, one, two, three, ..., depending on the length of the list in the following definition:
```haskell
data Rose a = Branch a [Rose a]
```
Notice that there is no empty rose tree, but there is a rose tree with a label and no subtrees (or rather an empty list of subtrees). For example, the size of a rose tree can be defined as follows, so that it is always a positive number:
```haskell
rsize :: Rose a -> Integer
rsize (Branch _ ts) = 1 + sum [rsize t | t <- ts]
```
This can be equivalently written as
```haskell
rsize' :: Rose a -> Integer
rsize' (Branch _ ts) = 1 + sum (map rsize' ts)
```
The height is a bit trickier. The perhaps obvious definition
```hs
rheight :: Rose a -> Integer
rheight (Branch _ ts) = 1 + maximum [rheight t | t <- ts] -- wrong
```
doesn't work because the maximum of the empty list is not defined. Here are two possible, equivalent definitions.
```haskell
rheight :: Rose a -> Integer
rheight (Branch _ []) = 0
rheight (Branch _ ts) = 1 + maximum [rheight t | t <- ts]

rheight' :: Rose a -> Integer
rheight' (Branch _ ts) = 1 + maximum (0 : [rheight' t | t <- ts])
```
The second definition can be understood as follows. In an expression `Branch x ts`, we call `x` the root of the tree and the list `ts` of subtrees a `forest`. Then the height of the forest `ts` is
 `maximum (0 : [rheight' t | t <- ts])`. In particular, if `ts` is empty then its height 0, with the same convention as before. But the height of `Branch x ts` is 1 plus the height of the forest `ts`, accounting for the root node `x`.

>  **Note:** The terminology "[rose tree](https://en.wikipedia.org/wiki/Rose_tree)" is popular in functional programming. In mathematics and other areas of computer science, these kinds of trees are more commonly referred to as a "rooted planar trees" or "rooted ordered trees".

<a name="gametrees"></a>
## Game trees

Suppose we have a type of boards and a type of moves, where for any given board we know what are the possible moves and the board that results from playing each of those moves.
Given an initial board, we can produce a game tree of all possible plays:
```haskell
data GameTree board move = Node board [(move, GameTree board move)] deriving (Show)

gameTree :: (board -> [(move,board)]) -> board -> GameTree board move
gameTree plays board = Node board [(m, gameTree plays b) | (m,b) <- plays board]
```
To make this a bit more concrete, let's apply it to the game of [Nim](https://en.wikipedia.org/wiki/Nim).
In this case, the "board" is a collection of heaps of objects that we represent as a list of `Integer`s (since only the counts matter), and a move consists of picking a particular heap and removing some number of objects, represented by a pair of an `Int` (for the index of the heap) and an `Integer` (for the number of objects to remove):
```haskell
type NimBoard = [Integer]
data NimMove = Remove Int Integer  deriving (Show,Eq)
```
The following "plays" function describes all the legal moves from a given Nim position:
```haskell
nimPlays :: NimBoard -> [(NimMove,NimBoard)]
nimPlays heaps = [(Remove i k, (hs ++ h-k : hs'))
                 | i <- [0..length heaps-1],
                   let (hs, h:hs') = splitAt i heaps,
                   k <- [1..h]]
```
By passing this as the first argument to `gameTree`, we can compute the entire game tree from a given Nim position:
```haskell
nim :: [Integer] -> GameTree NimBoard NimMove
nim = gameTree nimPlays
```
(Notice that this definition is "[point-free](https://wiki.haskell.org/Pointfree)" and that we've only partially applied the function `gameTree`.
The above definition is completely equivalent to the definition `nim initHeaps = gameTree nimPlays initHeaps`, just more concise.)

Let's try it out with Nim:
```hs
> nim [2]
Node [2] [(Remove 0 1,Node [1] [(Remove 0 1,Node [0] [])]),(Remove 0 2,Node [0] [])]
> nim [2,1]
Node [2,1] [(Remove 0 1,Node [1,1] [(Remove 0 1,Node [0,1] [(Remove 1 1,Node [0,0] [])]),(Remove 1 1,Node [1,0] [(Remove 0 1,Node [0,0] [])])]),(Remove 0 2,Node [0,1] [(Remove 1 1,Node [0,0] [])]),(Remove 1 1,Node [2,0] [(Remove 0 1,Node [1,0] [(Remove 0 1,Node [0,0] [])]),(Remove 0 2,Node [0,0] [])])]
> nim [1,1,1]
Node [1,1,1] [(Remove 0 1,Node [0,1,1] [(Remove 1 1,Node [0,0,1] [(Remove 2 1,Node [0,0,0] [])]),(Remove 2 1,Node [0,1,0] [(Remove 1 1,Node [0,0,0] [])])]),(Remove 1 1,Node [1,0,1] [(Remove 0 1,Node [0,0,1] [(Remove 2 1,Node [0,0,0] [])]),(Remove 2 1,Node [1,0,0] [(Remove 0 1,Node [0,0,0] [])])]),(Remove 2 1,Node [1,1,0] [(Remove 0 1,Node [0,1,0] [(Remove 1 1,Node [0,0,0] [])]),(Remove 1 1,Node [1,0,0] [(Remove 0 1,Node [0,0,0] [])])])]
```
Nim is usually played as a two-player game under ["misère"](https://en.wikipedia.org/wiki/Mis%C3%A8re) rules where the first player who can't make a move wins, but can also be played as a "normal" two-player game where the first player who can't make a move loses.
The question of whether or not the first player has a *winning strategy* from a given position can be entirely encapsulated into the logic of game trees.
```haskell
isWinning, isLosing :: Bool -> GameTree board move -> Bool
isWinning isMisere (Node b mgs)
        | null mgs  = isMisere
        | otherwise = any (isLosing isMisere)  [g | (m,g) <- mgs]
isLosing  isMisere (Node b mgs)
        | null mgs  = not (isMisere)
        | otherwise = all (isWinning isMisere) [g | (m,g) <- mgs]
```

Let's try it out:
```hs
> isWinning True (nim [2])
True
> isWinning True (nim [2,1])
True
> isWinning True (nim [1,1,1])
False
> isWinning False (nim [1,1,1])
True
```


All this deserves further discussion, of course.
For the purposes of these notes, we just wanted to illustrate yet-another-kind of tree that we may wish to define and manipulate in Haskell and in (functional or non-functional) programming more generally.

<a name="ptrees"></a>
## Permutation trees, list permutations, and paths in such trees (hard)

We now consider the type of list-branching trees, with labels on the edges rather than nodes, where a leaf is given by empty branching. What this means is that instead of having exactly two subtrees, as in binary trees, we have a (possibly empty) list of subtrees. If this list is empty, we have a leaf. The labels are at the edges, as in game trees, rather than at the nodes, as in the above binary trees:

```haskell
data Tree a = EBranch [(a, Tree a)] deriving (Show)
```
Then `EBranch []`, of type `Tree a`, is a leaf. The list of all paths from the root to leafs is constructed as follows:
```haskell
fullPaths :: Tree a -> [[a]]
fullPaths (EBranch []) = [[]]
fullPaths (EBranch forest) = [x:p | (x,t) <- forest, p <- fullPaths t]
```
A forest is an element of the type `[(a, Tree a)]`, namely a list of trees paired with elements. The list of all paths from the root to any node, including leafs:
```haskell
paths :: Tree a -> [[a]]
paths (EBranch forest) =  [] : [x:p | (x,t) <- forest, p <- paths t]
```
We now construct the permutation tree of a list, where the full paths of the tree are the permutations of the given list:
```haskell
permTree :: Eq a => [a] -> Tree a
permTree xs = EBranch [ (x, permTree(xs \\\ x)) | x <- xs]
  where
    (\\\) :: Eq a => [a] -> a -> [a]
    []     \\\ _   = undefined
    (x:xs) \\\ y
      | x == y     = xs
      | otherwise  = x : (xs \\\ y)
```

Using this, we can compute the list of all permutations of a given list:
```haskell
permutations :: Eq a => [a] -> [[a]]
permutations = fullPaths . permTree
```

You know that `n! = 1 * 2 * 3 * ... * n`. This is the [factorial](https://en.wikipedia.org/wiki/Factorial) of `n`, which counts the number of permutations of a list with `n` distinct elements. Hence one (inefficient!) way to compute the factorial function is
```haskell
factorial n = length(permutations [1..n])
```
The function `removals` defined below has bad time complexity (quadratic?). The function `removals2` defined below is conceptually more complicated but runs in linear time. It needs the function that given a list produces the list of pairs where the first component is a removed element and the second component is the given list with that element removed. We have two versions (one intuitively clear, and the other less clear but much faster):
```haskell
removals, removals2 :: [a] -> [(a,[a])]
removals [] = []
removals (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (removals xs)
```
Here is another representation of lists to make certain list operations
faster (called difference lists).
```haskell
type DList a = [a] -> [a]

removals' :: DList a -> [a] -> [(a,[a])]
removals' f [] = []
removals' f (x:xs) = (x, f xs) : removals' (f.(x:)) xs

removals2 = removals' (\xs -> xs)
```
With this, we can get permutation trees without relying on equality constraints:
```haskell
permTree2 :: [a] -> Tree a
permTree2 xs = EBranch [(y, permTree2 ys) | (y,ys) <- removals2 xs]

permutations2 :: [a] -> [[a]]
permutations2 = fullPaths . permTree2
```
But, as discussed above, it is much less clear why this algorithm should be correct (meaning that it does do what we have claimed it to do).

* Self-learning: find out about computing the list of permutations of a given list without going via trees.

* Convince yourself that our way with trees corresponds to some of the proposed ways without trees.

<a name="exprtrees"></a>
## Expression trees

Sometimes it is useful to work with a more specialised data type of trees tailored to a particular problem.
For instance, many compilers parse strings into *expression trees* and then process them to generate code.

Here we define a simple data type of numerical expression trees, together with an *evaluation* function that processes expression trees to produce values.
```haskell
data Expr a = Value a
            | FromInteger Integer
            | Negate (Expr a)
            | Abs (Expr a)
            | SigNum (Expr a)
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Value x)       = x
eval (FromInteger n) = fromInteger n
eval (Negate e)      = negate (eval e)
eval (Abs e)         = abs(eval e)
eval (SigNum e)      = signum(eval e)
eval (Add e e')      = eval e + eval e'
eval (Mul e e')      = eval e * eval e'
```
Before considering an example, this time let's define our own `show` function instead of asking Haskell to do this with the `deriving` mechanism.
To a first approximation, the class `Show` is defined as follows in the prelude:
```hs
class Show a where
  show :: a -> String
```
(In full detail, the `Show` class has [many more specified functions](https://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.Show.html#Show), but all of them can be derived from `show`.)
We define our `show` function for expression trees like so:
```haskell
instance Show a => Show(Expr a) where
  show (Value x)       = show x
  show (FromInteger n) = "fromInteger(" ++ show n ++ ")"
  show (Negate e)      = "negate(" ++ show e  ++ ")"
  show (Abs e)         = "abs(" ++ show e ++ ")"
  show (SigNum e)      = "signum(" ++ show e ++ ")"
  show (Add e e')      = "(" ++ show e ++ "+" ++ show e' ++ ")"
  show (Mul e e')      = "(" ++ show e ++ "*" ++ show e' ++ ")"
```
Example:
```hs
*Main> eval (Mul (Value 3) (Add (Value 7) (Value 6)))
39
*Main> Mul (Value 3) (Add (Value 7) (Value 6))
(3*(7+6))
*Main> show (Mul (Value 3) (Add (Value 7) (Value 6)))
"(3*(7+6))"
```
This version of the `show` function creates more brackets than necessary.
Can you get rid of them, by writing a better version of `show` that exploits operator precedence?

<a name="newtype"></a>
# Types with a single constructor

If a type has a single constructor with a single argument, it can be defined with `newtype`. See Section 8.3 of the book (page 95 of the printed version). However, there is a subtle, semantic difference with the corresponding declaration `data`, which is discussed in the [haskell wiki](https://wiki.haskell.org/Newtype).

If a data type has a single constructor and any number of arguments, it can be defined with *field labels*. Here is an example from [A Gentle Introduction to Haskell, Version 98](https://www.haskell.org/tutorial/moretypes.html):
```hs
data Point = Pt Float Float

pointx, pointy :: Point -> Float
pointx (Pt x _) = x
pointy (Pt _ y) = y
```
Using field labels, this can be equivalently defined as
```hs
data Point = Pt {pointx, pointy :: Float}
```
You can read this as `Point` has two fields, one called `pointx` and the other called `pointy`, both of type `Float`. Then both `Pt 1 2` and `Pt {pointx=1, pointy=2}` are allowed as (equivalent) values of this type. You can also use this in pattern matching:
```hs
norm (Pt {pointx = x, pointy = y}) = sqrt (x*x+y*y).
```
There is also an *update syntax*: For  example, if `p` is a point, then `p {pointx = 2}` is a new point with the field `pointx` replaced by `2` and the same `pointy` field.