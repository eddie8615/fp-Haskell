# Formative assignment 3

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

* Go into the "Assignments/Formative/Formative3" directory and *copy* the given `Formative3Template.hs` to a *new* file `Formative3.hs`.
  Work on that file to produce your solution and then submit it to Canvas.

  It is important to copy the file as explained, since any future `git pull` may overwrite the template. Hence don't work directly on the template.

* Be aware that:

  * Your solutions must work with GHC 8.6.5. To use GHC 8.6.5 on a lab machine, see [HardwareAndSoftware.md](../../Resources/HardwareAndSoftware.md).

  * If you wish to import modules, then you may only import libraries from [the standard library](http://hackage.haskell.org/package/base). Additionally, all modules you import must be "Safe" on Hackage.

## Background

### Word squares and word rectangles

A **word square** is a square matrix of characters, where each of the rows and columns corresponds to a word.
For example,
```
CODE
OVAL
SETS
TREE
```
and
```
DATA
AJAX
FALL
TREE
```
are two 4x4 word squares, while
```
TYPES
REACT
UNCLE
STEAM
TARTS
```
is a 5x5 word square (note that [yenta](https://en.wiktionary.org/wiki/yenta) and [eclat](https://en.wiktionary.org/wiki/eclat) are [loan words](https://en.wikipedia.org/wiki/Loanword)).
The words of a word square do not necessarily have to be distinct, and they can be constructed given a dictionary for any alphabetic language.
For example,

```
CAOS
ARTE
OTRA
SEAN
```

is a valid 4x4 word square in Spanish.

The idea of a word square may be naturally generalised to define *word rectangles*: a **word rectangle** is an m x n matrix of characters, where each of the m rows and n columns corresponds to a word.
Here are some examples of word rectangles:
```
LIST
ANEW
RAVE
KNEE
SENT
```
(A 5x4 word rectangle.)
```
LAMBDA
ADROIT
DOSAGE
```
(A 3x6 word rectangle.)
```
MONAD
URINE
MALTS
SLEEK
```
(A 4x5 word rectangle.)

## Constructing word rectangles using backtracking and tries

In [volume 4, pre-fascicle 5B](https://www-cs-faculty.stanford.edu/~knuth/fasc5b.ps.gz) of *The Art of Computer Programming*, Knuth suggests a procedure for constructing m x n word rectangles using backtracking.
The idea is to grow the rectangle by adding n-letter words one row at a time, ensuring that at each step, each column is a valid *prefix* of an m-letter word.
For example, suppose we are trying to construct a 4x5 rectangle, and have the following partial solution:

```
MONAD
URINE
```

This is a valid *partial* solution, since although the strings `MU`, `OR`, `NI`, `AN`, `DE` are not valid 4-letter words, they are valid prefixes of 4-letter words.
We can try to make progress by adding a row containing another 5-letter word, say `CABAL`:

```
MONAD
URINE
CABAL
```

This is still a valid partial solution, since all of the strings `MUC`, `ORA`, `NIB`, `ANA`, and `DEL` are valid prefixes of 4-letter words.
However, at this point we get stuck since there is no way to add another row to get a word rectangle (at least not in our dictionary).
So, we backtrack and try another word for the third row:

```
MONAD
URINE
SANTA
```

Now we can complete the rectangle by adding a fourth row (happily, our dictionary contains proper nouns):

```
MONAD
URINE
SANTA
KLEIN
```


To make this procedure more efficient, Knuth suggests using a [trie](https://en.wikipedia.org/wiki/Trie) data structure in order to quickly test for prefixes.
(He also makes a few other suggestions.)

# Questions

In this formative assignment, you will write functions for constructing word rectangles from a given dictionary.
The specific choice of algorithms and data structures is up to you, but we suggest using backtracking for correctness and completeness, and using tries for efficiency.
Even within those parameters, there is a lot of room for creativity, and we will run a competition to determine the best solutions under various criteria, with the winners being awarded virtual trophies on Slack.
(The evaluation criteria will certainly involve efficiency, and perhaps other factors. The precise parameters of the competition will be described in due time.)

You can make use of the following routine, which takes the path to a dictionary file containing a list of words (one per line) and returns the corresponding list, with all of the words converted to upper case letters:
```haskell
import Data.Char

readDict :: String -> IO [String]
readDict path = do
  content <- readFile path
  return (map (map toUpper) $ lines content)
```

We have provided a variety of dictionaries in the Git repository that you can use for testing purposes:
* [`common100.txt`](common100.txt): list of 100 most common words in English
* [`ogden850.txt`](ogden850.txt): Ogden's Basic English
* [`common10000.txt`](common10000.txt): list of 10000 most common words in English
* [`english2.txt`](english2.txt): a longer list of words in English (65199 words)
* [`spanish5000.txt`](spanish5000.txt): list of 5000 words in Spanish

On most Unix systems (such as the lab machines) there is also a standard dictionary file, available at `/usr/share/dict/words`.
Feel free to download or create additional dictionaries for your own testing purposes.
(It will probably be a good idea to begin by testing your code on very small dictionaries.)

1. In the first part of the assignment you will implement a trie data structure for storing strings and efficiently testing for prefixes.
   The specific representation is up to you:
   ```haskell
   type Trie = ()   -- replace this by an appropriate type, data, or newtype definition of your choice
   ```
   but your `Trie` should support at least the following operations:
   ```haskell
   -- construct the empty trie
   emptyTrie :: Trie
   emptyTrie = undefined

   -- insert a string into a trie
   insertTrie :: String -> Trie -> Trie
   insertTrie = undefined

   -- given a trie, return the list of strings it contains
   trieToList :: Trie -> [String]
   trieToList = undefined

   -- given a string xs and a trie t, return a (possibly empty) trie containing all of the possible suffixes of xs in t
   findTrie :: String -> Trie -> Trie
   findTrie = undefined

   -- return True if the trie is empty, or else False
   isEmpty :: Trie -> Bool
   isEmpty = undefined

   -- return True if the trie contains the empty string "", or else False
   epsilon :: Trie -> Bool
   epsilon = undefined
   ```

   Examples:
   ```hs
   > words <- readDict "common100.txt"
   > t = foldr insertTrie emptyTrie words
   > trieToList (findTrie "THE" t)
   ["","Y","RE","IR","M ","N","SE"]
   > epsilon (findTrie "THE" t)
   True
   > isEmpty (findTrie "X" t)
   True
   ```

2. Write a function
   ```haskell
   wordrects :: [String] -> Int -> Int -> [String] -> [[String]]
   wordrects dict rows cols crib = undefined
   ```
   which given a list of words `dict`, numbers `rows` and `cols`, and a list of strings `crib`, returns all of the `rows`x`cols` word rectangles extending `crib`.
   You can assume that `length crib <= rows` and that `all ((== cols) . length) crib`.

   As already mentioned, we recommend that you implement `wordrects` using the backtracking approach described above (you can study the continuations-based [SAT solver](../../../LectureNotes/satsolver.md) we discussed in class as a model), building on the trie data structure you already implemented above.
   However, the specific strategy is entirely up to you, and you are encouraged to think about how to make `wordrects` as efficient as possible (you can look in [Knuth's notes](https://www-cs-faculty.stanford.edu/~knuth/fasc5b.ps.gz) for some more ideas about optimising the algorithm and data structures).
   Of course, you should only do this **after** ensuring that your implementation is both sound and complete.
   Here by "soundness" and "completeness" we mean that:
   
   * *soundness*: every rectangle in the list returned by `wordrects` is a valid `rows`x`cols` word rectangle extending `crib`, using words from `dict`;
   * *completeness*: the list returned by `wordrects` contains all of the valid `rows`x`cols` word rectangles extending `crib` that can be constructed using words from `dict`.
   
   Examples:
   ```hs
   > words <- readDict "common100.txt"
   > wordrects words 2 2 []
   [["TO","OF"],["TO","ON"],["TO","OR"],["IN","TO"],["IN","NO"],["IT","TO"],["IT","NO"],["ON","NO"],["AS","TO"],["AS","SO"],["AS","NO"],["DO","OF"],["DO","ON"],["DO","OR"],["AT","TO"],["AT","SO"],["AT","NO"],["AN","TO"],["AN","SO"],["AN","NO"],["SO","OF"],["SO","ON"],["SO","OR"],["GO","OF"],["GO","ON"],["GO","OR"],["NO","OF"],["NO","ON"],["NO","OR"],["US","SO"]]
   > wordrects words 2 2 ["DO"]
   [["DO","OF"],["DO","ON"],["DO","OR"]]
   > words <- readDict "english2.txt"
   > wordrects words 4 5 ["MONAD","URINE"]
   [["MONAD","URINE","MALTS","SLEEK"],["MONAD","URINE","SANTA","KLEIN"]]
   ```
   
## Can you enter the efficiency contest?
To find out if you can enter the efficiency contest, you need to do the following.

Log in on a **lab machine**. In a terminal, run
```
$ cd fp-learning-2019-2020/Assignments/Formative/Formative3
$ git pull
$ module load func-prog
```
to get the right setup.

Make sure that your solutions are in `Formative3.hs`. Now do
```
$ ghc -O2 EntryRequirement.hs
```
to compile the eligibility test (and your `Formative3.hs` file). Note that you will need to redo this step if you change your `Formative3.hs` file.

Finally, run
```
$ time ./EntryRequirement
```
The output of this command will either look like this:
```
You can enter the competition if your script was faster than 30 seconds.

4.632u 0.021s 0:04.67 99.5%	0+0k 0+0io 0pf+0w
```
or like this:
```
Unfortunately, your script is incorrect and so your script is not eligible for competition.

4.632u 0.021s 0:04.67 99.5%	0+0k 0+0io 0pf+0w
```

In the first case, you can check the running time of your script by inspecting the third column of the final line of output (it reads `0:04.67` in this case). It displays the running time in `minute:seconds.milliseconds` format. If it's less than 30 seconds, then you can enter the competition.
