# Introduction to functional programming

Here we give a brief introduction to some of the topics we will discuss in the module.

# Functional programming

Functional programming is a style of programming that can be practised in virtually any programming language, and more and more conventional programming languages are adding support for it, including [Java](https://www.google.com/search?client=ubuntu&channel=fs&q=functional+programming+java&ie=utf-8&oe=utf-8).

Some languages, including [ML](https://en.wikipedia.org/wiki/ML_(programming_language)), [Miranda](https://en.wikipedia.org/wiki/Miranda_(programming_language)), [Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language)), [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)), [OCaml](https://en.wikipedia.org/wiki/OCaml), [F#](https://en.wikipedia.org/wiki/F_Sharp_(programming_language)), [Agda](https://en.wikipedia.org/wiki/Agda_(programming_language)), are explicitly designed for that purpose, and they are called *functional programming languages*.

In this module we will use Haskell, and perhaps a little bit of Agda, but the ideas are meant to apply to any programming language. The goal of this module is to learn *functional programming*, and hence the name of the module. However, you are expected to learn Haskell too, and you will be examined in Haskell and the coursework will be in Haskell.

Functional languages are sometimes used directly in industry. One example is [WhatsApp](https://www.wired.com/2015/09/whatsapp-serves-900-million-users-50-engineers/). Some of our former students got jobs or internships in functional programming, for example at [Barclays](http://neilmitchell.blogspot.co.uk/2016/09/full-time-haskell-jobs-in-london-at.html). More recently, Google has adopted [Kotlin](https://kotlinlang.org/) as the [preferred](https://techcrunch.com/2019/05/07/kotlin-is-now-googles-preferred-language-for-android-app-development/) language for [Android development](https://developer.android.com/kotlin). This is a language in the Java ecosystem which directly supports functional programming (and Java itself has incorporated a number of functional programming idioms in the last few years).

# Functional programming is good for modularity

[This classical paper](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf), published in 1990, explains this very eloquently. There is also a [talk by the author](https://www.google.com/search?client=ubuntu&hs=KH9&channel=fs&q=why+functiona+programming+matters+video&oq=why+functiona+programming+matters+video&gs_l=psy-ab.3..33i160k1.15318.16097.0.16271.6.6.0.0.0.0.99.379.5.5.0....0...1.1.64.psy-ab..1.5.377...0i13k1j0i22i30k1j33i21k1.0.stQ_hRkTWR0), from 2017.

# Quick Haskell overview

## Haskell notation for lists

* `[]` is the empty list.

* `[1,2,3,4,5]` is a list.

* `[1..5]` is a list, equal to `[1,2,3,4,5]`.

* `0 : [1..5] = [0,1,2,3,4,5]`. The symbol `:` (called *cons*) is for adding a new head to a list.

* A one-element list can be written `[3]`, which is equivalent to `3 : []`.

* `[1,2,3,4,5] ++ [100,101,102] = [1,2,3,4,5,100,101,102]`. The symbol `++` is for concatenation of two lists.

* The `head` of a non-empty list is its first element, its `tail` is the list of remaining elements.

* `head(x:xs) = x`

* `tail(x:xs) = xs`

* `head [1..1000000] = 1`

* `tail [1..1000000] = [2..1000000]`

* `last [1..1000000] = 1000000`

* The type of lists of integers is `[Integer]`.

* The type of lists of machine-size integers is `[Int]`.

* The type of lists of elements of a type `a` is `[a]`. Notice that `a` is a type variable.

* `Char` is the type of characters. The type `String` of strings is a synonym for `[Char]`.

* There are also *list comprehensions*, which we will discuss later, and appear briefly below.

* To import a library, include an `import` statement at the top of the source file. For example, the statement
  ```hs
  import Data.Char
  ```
  loads [various routines](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html) for dealing with Unicode characters.

## Pattern matching on lists, and guards

If we use letters `x`, `y`, `z` for *elements* of lists, then the usual convention is to use plural letters `xs`, `ys`, `xs` for lists. The quadratic-time reverse function can be defined as follows:

```haskell
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]
```
Here we have:

* `::` is used for type declarations. Don't confuse it with the *cons* function `:` discussed above, which is for adding a new head to a list.

* After the type declaration of the function `rev`, we define it by two equations.

* Notice that we create the list `[x]` with just one element, and then we concatenate it to a recursive call with xs, with `++` rather than `:`, of course.

We can also use `case` for pattern matching:

```haskell
rev' :: [a] -> [a]
rev' xs = case xs of
           []     -> []
           (x:xs) -> rev' xs ++ [x]
```
I prefer the first version without `case`, but `case` is useful when we want to pattern match in the middle of a definition.


As another example, a version of quick sort can be written as follows:

```haskell
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =
     qsort [l | l <- xs, l <= x]
  ++ [x]
  ++ qsort [r | r <- xs, r > x]
```
Here we have:

* The things that come before `=>` in the type declaration are type constraints. In this particular example, the constraint is that the type `a` must be in the class `Ord` of types that admit an ordering.

* The expression `[l | l <- xs, l <= x]` is a *list comprehension*, which generates the list of elements smaller-than-or-equal-to `x`.

Merging two sorted lists so that the output is sorted, as needed for the merge-sort algorithm:

```haskell
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys
```
We can equivalently write this with *guards* as follows:

```haskell
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys)
  | x <= y    = x : merge' xs (y:ys)
  | otherwise = y : merge' (x:xs) ys
```
## Side-effects

Programs have an inputs and and outputs, and perhaps a side-effect, such as printing, writing a file, reading from the internet, changing state. Haskell confines side-effects to so-called monadic types, such as [IO()](http://learnyouahaskell.com/input-and-output).

```haskell
hello :: IO()
hello = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
```
On the other hand, you can't print or read in a function of type `Integer -> Integer`. If you did want to have an IO side-effect in such a function, its type would have to be `Integer -> IO Integer` instead, indicating that in addition to outputting an integer, you will have IO.

## Laziness

Haskell is *call-by-need* or *lazy*, and other languages may be *call-by-name*, *call-by-value*, *call by reference*. (You can learn about this in a module called [*Principles of programming languages*
(http://www.cs.bham.ac.uk/internal/modules/2019/06-26954/)

```haskell
bigInteger :: Integer
bigInteger = last [1..10000000000000000]

f :: Integer -> Integer
f x = 17

example1 :: IO()
example1 = putStrLn (show(f bigInteger))
```
This works as follows: the expression `[1..10000000000000000]` produces the list of numbers from 1 to 10000000000000000, one by one. This is a big list, and won't fit in your computer. the function `last` takes the last element of this list. This will take a long time, but won't exhaust memory, because Haskell is lazy, and will create the elements of the list on demand. Because Haskell is lazy, `bigInteger` (which is 10000000000000000), won't be evaluated until it is needed. The function `f` doesn't use its argument, and hence in the call `f bigInteger`, the argument `bigInteger` is not evaluated. So the function `example1` prints 17 very quickly, without ever evaluating `bigInteger`. An equivalent program in a call-by-value language such as Java wouldn't work in practice, as the call `f bigInteger` would attempt to evaluate `bigInteger` first.

The above is a silly example. One use of laziness is to read a whole, large file, then process it, then output. Because of laziness, the whole file won't be read at once, but on demand, automatically.

Another use of laziness is to work with infinite objects, such as lists. For example, `[0..]` is the infinite list `[0,1,2,3,...`. Its evaluation never terminates. However, `take 5 [0..]` evaluates to `[0,1,2,3,4]` in finite time. Here

```haskell
from :: Enum a => a -> [a]
from n = n : from(succ n)
```
## Full example

This code converts a markdown file like this to a Haskell file, by extracting the code only:

```hs
import Data.Char

begin = "```haskell"
end   = "```"

mdtohs :: [String] -> String
mdtohs [] = []
mdtohs (xs:xss)
  | take (length begin) (dropWhile isSpace xs) == begin  = copy (length (takeWhile isSpace xs)) xss
  | otherwise                                            = mdtohs xss

copy :: Int -> [String] -> String
copy i [] = []
copy i (xs:xss)
  | take (length end) (dropWhile isSpace xs) == end  = "\n" ++ mdtohs xss
  | otherwise                                        = drop i xs ++ "\n" ++ copy i xss

main :: IO()
main = interact(mdtohs.lines)
```
The function `lines :: String -> [String]` converts a string into a list of strings, one for each line terminated by "\n". The function `interact :: (String -> String) -> IO ()` converts a function `String->String` into a program of type `IO()` which reads from the standard input (stdin) into the standard output (stdout). For example, to generate the file `Introduction.hs` from the file `Introduction.md`, I run, under a unix like system:
```
$ cat Introduction.md | runhaskell mdtohs.hs > Introduction.hs
```
* The cat command echoes the file Introduction.md to stdout.

* The unix pipe `|` makes the standard output of the previous thing into the standard input of the next thing.

* Then we run Haskell with the file `mdtohs.hs` containing the above conversion code.

* The linux redirection `>` saves the output of the previous thing to the file named after it, namely [`Introduction.hs`](https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018/blob/master/LectureNotes/Introduction.hs) in this case.
