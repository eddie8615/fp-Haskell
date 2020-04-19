# A continuations-based backtracking SAT solver

In these notes we develop a backtracking solver for the [Boolean satisfiability problem (SAT)](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem), based on the technique of [success and failure continuations](continuations.md#succfail).
It is intended both as an example of the power of continuations to express a relatively sophisticated algorithm in concise notation, and as an introduction to the technique of backtracking, which can be used to solve many other similar kinds of combinatorial search problems.

# Contents

* [Background: the SAT problem](#background)
* [Encoding the syntax of formulas](#syntax)
* [Encoding the semantics of formulas](#semantics)
* [The backtracking SAT solver](#solver)
* [Applications](#applications)
  * [k-vertex-colouring](#kcoloring)
  * [The n-queens problem](#nqueens)
  * [Tautology checking](#tautology)
* [Backtracking using the (lazy) list monad](#listmonad)

<a name="background"></a>
## Background: the SAT problem

Let's begin by reminding ourselves about the SAT problem.
The syntax of Boolean formulas is defined as follows, in BNF notation:

```
P,Q ::= X,Y,Z,...  (atomic variables)
      | P /\ Q     (conjunction)
      | P \/ Q     (disjunction)
      | ~ P        (negation)
      | TT         (constant true)
      | FF         (constant false)
```

A formula is said to be *satisfiable* if there is some assignment of truth values (`TT` or `FF`) to the atomic variables making the whole formula true.
For example, the formula

```
(X \/ Y) /\ (~ X \/ ~ Y)
```

has two satisfying assignments, namely
```
X |-> TT
Y |-> FF
```
and
```
X |-> FF
Y |-> TT
```
On the other hand, the formula 
```
X /\ ~ X
```
has no satisfying assignment.

As you will remember, the problem of deciding whether a general Boolean formula is satisfiable is NP-complete.
Indeed, many different problems may be naturally reduced to SAT.
As one example, let's consider the problem of deciding whether a graph has a *k-vertex-colouring*, i.e., whether it is possible to assign every vertex a colour such that no pair of adjacent vertices has the same colour, and such that the total number of distinct colours used is less than or equal to k.
For instance, consider the following graph containing six vertices and the indicated edges:

```
      3
     /|\
    / | \
   /  |  \
  /   5   \
 |   / \   |
 |  4---6  |
 | /     \ |
 |/       \|
 1---------2
```
We can encode the question of whether this graph has a 3-colouring as the following SAT formula:
```
(R1 \/ G1 \/ B1) /\ 
(R2 \/ G2 \/ B2) /\
(R3 \/ G3 \/ B3) /\
(R4 \/ G4 \/ B4) /\
(R5 \/ G5 \/ B5) /\
(R6 \/ G6 \/ B6) /\
~ (R1 /\ R2) /\ ~ (G1 /\ G2) /\ ~ (B1 /\ B2) /\ 
~ (R1 /\ R3) /\ ~ (G1 /\ G3) /\ ~ (B1 /\ B3) /\ 
~ (R1 /\ R4) /\ ~ (G1 /\ G4) /\ ~ (B1 /\ B4) /\ 
~ (R2 /\ R3) /\ ~ (G2 /\ G3) /\ ~ (B2 /\ B3) /\ 
~ (R2 /\ R6) /\ ~ (G2 /\ G6) /\ ~ (B2 /\ B6) /\ 
~ (R3 /\ R5) /\ ~ (G3 /\ G5) /\ ~ (B3 /\ B5) /\ 
~ (R4 /\ R5) /\ ~ (G4 /\ G5) /\ ~ (B4 /\ B5) /\ 
~ (R4 /\ R6) /\ ~ (G4 /\ G6) /\ ~ (B4 /\ B6) /\ 
~ (R5 /\ R6) /\ ~ (G5 /\ G6) /\ ~ (B5 /\ B6)
```

What we've done here is begin by introducing 6*3 = 18 variables, standing for the fact that a vertex is assigned a given colour (e.g., `R3` stands for the proposition that vertex 3 is coloured red, `G4` for the proposition that vertex 4 is coloured green, etc.).
Then we introduced a clause of the form
```
Rx \/ Gx \/ Bx
```
for every vertex x ("x is either red, green, or blue"), as well as a clause of the form
```
~ (Rx /\ Ry) /\ ~ (Gx /\ Gy) /\ ~ (Bx /\ By)
```
for every edge (x,y) ("x and y are not assigned the same colour"), taking the big conjunction of all such clauses.
An example of a satisfying assignment is the one setting the variables
```
R1, G2, B3, G4, R5, B6
```
to `TT`, and all the other variables to `FF`.
In turn, this corresponds to the following 3-colouring of the original graph:
```
      B
     /|\
    / | \
   /  |  \
  /   R   \
 |   / \   |
 |  G---B  |
 | /     \ |
 |/       \|
 R---------G
```

<a name="syntax"></a>
## Encoding the syntax of formulas

To encode the syntax of Boolean formulas, we introduce the following data type:
```haskell
type Atm = String
data Frm = Var Atm | And Frm Frm | Or Frm Frm | Not Frm | TT | FF
     deriving (Show,Eq)
```
For simplicity, we are representing atomic variables as strings.

We also introduce some "syntactic sugar" for additional logical connectives defined in terms of the ones above, which will be convenient in examples:

```haskell
(==>) :: Frm -> Frm -> Frm
(==>) p q = Not p `Or` q

bigAnd, bigOr :: [Frm] -> Frm
bigAnd = foldr And TT
bigOr  = foldr Or  FF
```

<a name="semantics"></a>
## Encoding the semantics of formulas

To encode the semantics of formulas, we define here a function `evalFrm p rho` for evaluating a given Boolean formula `p` relative to an assignment `rho` of values for atomic variables:
```haskell
type Asn = [(Atm,Bool)]

evalFrm :: Frm -> Asn -> Bool
evalFrm (Var x)   rho = case lookup x rho of
                          Just v  -> v
                          Nothing -> False      -- could also return True here, or raise an error
evalFrm (And p q) rho = evalFrm p rho && evalFrm q rho
evalFrm (Or p q)  rho = evalFrm p rho || evalFrm q rho
evalFrm (Not p)   rho = not (evalFrm p rho)
```
Since our backtracking solver naturally produces *partial* assignments, where only a subset of the variables may be assigned values, we find it convenient to return a default value `False` whenever a variable does not occur in `rho`.
We could just as well return `True` here, or alternatively raise an error, but the latter would be less convenient because we wouldn't be able to evaluate formulas relative to partial assignments.

<a name="solver"></a>
## The backtracking SAT solver

The "SAT solving" problem can be formulated in various slightly different ways, given a Boolean formula P:

* Does there *exist* a satisfying assignment for P? (The decision problem for SAT.)
* Can we *find* some satisfying assignment for P?
* Can we find the list of *all* satisfying assignments for P?

A naive approach to SAT solving in any of these forms is to simply try all of the 2^n possible assignments, where n is the number of distinct variables in P.
Since the decision problem for SAT is NP-complete, in some sense we shouldn't hope to do much better than this exponential time algorithm.
However, in practice there are much better approaches, and modern SAT solvers are capable of dealing with formulas with hundreds of thousands or even millions of variables coming from industrial applications.
The approach we will be considering is a lot more naive than the state-of-the-art approaches, but it still serves as a good basis for optimisation, and is sufficiently flexible to adapt to many other kinds of combinatorial search problems.
It is similar in spirit to the classic [DPPL algorithm](https://en.wikipedia.org/wiki/DPLL_algorithm), which works on formulas in conjunctive normal form.

As motivation for why we should be able to do better than the brute force approach of trying all 2^n possible assignments, consider a formula consisting of a conjunction of atoms and negated atoms, such as:
```
X /\ ~Y /\ ~Z /\ W
```
It is easy to see that this formula has exactly one satisfying assignment,
```
X |-> TT
Y |-> FF
Z |-> FF
W |-> TT
```
and we should not have to consider all 2^4=16 possible assignments to recognise this.
Indeed, whenever we have a formula of the form
```
X /\ ...
```
we can tell that any satisfying assignment will have to assign `X |-> TT` (in addition to assigning values to the other variables to satisfy the rest of the formula), and similarly, if we have a formula of the form
```
~Y /\ ...
```
we can tell that any satisfying assignment will have to assign `Y |-> FF`.
In this way, by progressively examining the different conjuncts of a formula, we can gradually accumulate more and more constraints on the variables, until either we're done and we have a satisfying assignment for the whole formula, or we've reached a conflicting set of constraints, in which case we know that the formula is unsatisfiable.
Of course, a wrinkle in this plan is that the formula may also contain disjunctions,
```
P \/ Q
```
which can lead to different possible sets of constraints.
This is where backtracking comes in: we begin by trying to satisfy one of the formulas (say P), and if that fails we backtrack and try the other formula.

With that by way of high-level description, here is the (very concise!) code for our backtracking SAT solver in continuation-passing style, based on the technique of [success and failure continuations](continuations.md#succfail):
```haskell
satisfy,falsify :: Frm -> Asn -> (Asn -> r -> r) -> r -> r
satisfy (Var x)   rho succ fail = case lookup x rho of
                                    Just v -> if v == True then succ rho fail else fail
                                    Nothing -> succ ((x,True):rho) fail
satisfy (And p q) rho succ fail = satisfy p rho (\rho' fail' -> satisfy q rho' succ fail') fail
satisfy (Or p q)  rho succ fail = satisfy p rho succ (satisfy q rho succ fail)
satisfy (Not p)   rho succ fail = falsify p rho succ fail
satisfy TT        rho succ fail = succ rho fail
satisfy FF        rho succ fail = fail

falsify (Var x)   rho succ fail = case lookup x rho of 
                                    Just v -> if v == False then succ rho fail else fail
                                    Nothing -> succ ((x,False):rho) fail
falsify (And p q) rho succ fail = falsify p rho succ (falsify q rho succ fail)
falsify (Or p q)  rho succ fail = falsify p rho (\rho' fail' -> falsify q rho' succ fail') fail
falsify (Not p)   rho succ fail = satisfy p rho succ fail
falsify TT        rho succ fail = fail
falsify FF        rho succ fail = succ rho fail
```
Take some time to study these definitions!
(We will in class.)

By passing appropriate success and failure continuations to `satisfy`, we can define functions for the different variations on the SAT solving problem mentioned at the start of this section.

**Exercise**:
Define functions
```
isSatisfiable :: Frm -> Bool
someSolution :: Frm -> Maybe Asn
allSolutions :: Frm -> [Asn]
```
with the intended specifications, by calling `satisfy` with the appropriate parameters.

<details><summary>(Answer for isSatisfiable)</summary>

```haskell
isSatisfiable p = satisfy p [] (\_ _ -> True) False
```
</details>

<details><summary>(Answer for someSolution)</summary>

```haskell
someSolution p = satisfy p [] (\s _ -> Just s) Nothing
```
</details>

<details><summary>(Answer for allSolutions)</summary>

```haskell
allSolutions p = satisfy p [] (:) []
```
</details>

<a name="applications"></a>
## Applications

We show here a few examples of how to combine our SAT solver with encodings of different problems into SAT (including the graph colouring problem already mentioned in the introduction).
Of course there are other ways of approaching these problems (e.g., by writing backtracking solvers specialised to these domains), but part of the point is also to illustrate how the encodings themselves can be efficiently programmed in Haskell.

<a name="kcoloring"></a>
### k-vertex-colouring

Function to encode a k-colouring problem as a Boolean formula:

```haskell
kcolor :: Int -> [Int] -> [(Int,Int)] -> Frm
kcolor k verts edges =
   bigAnd [bigOr [hascolour x c | c <- [1..k]] | x <- verts] `And`
   bigAnd [Not (hascolour x c `And` hascolour y c) | (x,y) <- edges, c <- [1..k]]
   where
     hascolour x c = Var ("C" ++ show (x,c))
```

Example from introduction:

```
      3
     /|\
    / | \
   /  |  \
  /   5   \
 |   / \   |
 |  4---6  |
 | /     \ |
 |/       \|
 1---------2
```
```haskell
prismGraph = kcolor 3 [1..6] [(1,2),(1,3),(1,4),(2,3),(2,6),(3,5),(4,5),(4,6),(5,6)]
```

Sample interaction with SAT solver:
```hs
> someSolution prismGraph
Just [("C(6,1)",False),("C(4,3)",False),("C(5,2)",False),("C(5,3)",False),("C(6,2)",False),("C(2,3)",False),("C(3,2)",False),("C(4,1)",False),("C(3,1)",False),("C(1,3)",False),("C(1,2)",False),("C(2,1)",False),("C(6,3)",True),("C(5,1)",True),("C(4,2)",True),("C(3,3)",True),("C(2,2)",True),("C(1,1)",True)]
> filter snd <$> someSolution prismGraph
Just [("C(6,3)",True),("C(5,1)",True),("C(4,2)",True),("C(3,3)",True),("C(2,2)",True),("C(1,1)",True)]
```

Corresponding coloring:
```
      B
     /|\
    / | \
   /  |  \
  /   R   \
 |   / \   |
 |  G---B  |
 | /     \ |
 |/       \|
 R---------G
```


<a name="nqueens"></a>
### The n-queens problem

(See [eight queens puzzle](https://en.wikipedia.org/wiki/Eight_queens_puzzle).)

Function to encode an n-queens problem as a Boolean formula:

```haskell
queens :: Int -> Frm
queens n =
  bigAnd [bigOr [queenAt (i,j) | j <- [1..n]] | i <- [1..n]] `And`
  bigAnd [queenAt x ==>
          bigAnd [Not (queenAt y) | y <- cells, x /= y, sameRow x y || sameCol x y || sameDia x y] | x <- cells]
  where
    cells = [(i,j) | i <- [1..n], j <- [1..n]]
    queenAt (i,j) = Var ("Q" ++ show (i,j))
    sameRow (i1,j1) (i2,j2) = i1 == i2
    sameCol (i1,j1) (i2,j2) = j1 == j2
    sameDia (i1,j1) (i2,j2) = abs(i2-i1) == abs(j2-j1)
```

Sample interaction with SAT solver: (with profiling and comments):
```hs
*Main> :set +s
*Main> filter snd <$> someSolution (queens 4)
Just [("Q(4,3)",True),("Q(3,1)",True),("Q(2,4)",True),("Q(1,2)",True)]
(0.02 secs, 918,384 bytes)
-- Corresponding chessboard:
-- .Q..
-- ...Q
-- Q...
-- ..Q.
*Main> filter snd <$> someSolution (queens 5)
Just [("Q(5,4)",True),("Q(4,2)",True),("Q(3,5)",True),("Q(2,3)",True),("Q(1,1)",True)]
(0.01 secs, 2,153,448 bytes)
-- Corresponding chessboard:
-- Q....
-- ..Q..
-- ....Q
-- .Q...
-- ...Q.
*Main> filter snd <$> someSolution (queens 6)
Just [("Q(6,5)",True),("Q(5,3)",True),("Q(4,1)",True),("Q(3,6)",True),("Q(2,4)",True),("Q(1,2)",True)]
(0.23 secs, 97,934,168 bytes)
-- Corresponding chessboard:
-- .Q....
-- ...Q..
-- .....Q
-- Q.....
-- ..Q...
-- ....Q.
*Main> filter snd <$> someSolution (queens 7)
Just [("Q(7,6)",True),("Q(6,4)",True),("Q(5,2)",True),("Q(4,7)",True),("Q(3,5)",True),("Q(2,3)",True),("Q(1,1)",True)]
(0.59 secs, 256,154,568 bytes)
-- Corresponding chessboard:
-- Q......
-- ..Q....
-- ....Q..
-- ......Q
-- .Q.....
-- ...Q...
-- .....Q.
*Main> filter snd <$> someSolution (queens 8)
Just [("Q(8,4)",True),("Q(7,2)",True),("Q(6,7)",True),("Q(5,3)",True),("Q(4,6)",True),("Q(3,8)",True),("Q(2,5)",True),("Q(1,1)",True)]
(29.03 secs, 12,027,583,056 bytes)
-- Corresponding chessboard:
-- Q.......
-- ....Q...
-- .......Q
-- .....Q..
-- ..Q.....
-- ......Q.
-- .Q......
-- ...Q....
*Main> [length (allSolutions $ queens n) | n <- [0..7]]
[1,1,0,0,2,10,4,40]    -- See https://oeis.org/A000170
(29.47 secs, 14,447,835,248 bytes)
```

<a name="tautology"></a>
### Tautology checking and the pigeonhole principle

A *tautology* is a formula that remains true under any assignment of truth values to the variables, which is equivalent to asking that its negation is not satisfiable:

```haskell
isTautology :: Frm -> Bool
isTautology = not . isSatisfiable . Not
```

Here are some simple examples of tautologies:

```hs
> isTautology (Var "P" ==> Var "P")
True
> isTautology (((Var "P" ==> Var "Q") `And` (Var "Q" ==> Var "R")) ==> (Var "P" ==> Var "Q"))
True
```

And an example of a formula that is not a tautology, although it is satisfiable:

```hs
> isTautology ((Var "P" `Or` Var "Q") ==> Var "P")
False
```

A much more interesting example of a tautology is the *propositional pigeonhole principle*.

As you may remember, in mathematics, the [pigeonhole principle](https://en.wikipedia.org/wiki/Pigeonhole_principle) says that if you place m pigeons into n holes, and m > n, then there must be at least two pigeons placed in the same hole.
Or more abstractly: *if f is a function from an m-element set to an n-element set, and m > n, then f must take the same value on two different elements*.

The pigeonhole principle can also be translated into propositional logic as follows.
For every m and n, we define a formula PHP(m,n) expressing, "if m pigeons are placed into n holes, then there must be at least two pigeons placed in the same hole":

```haskell
php :: Int -> Int -> Frm
php m n = bigAnd [bigOr [pigeon p h | h <- [1..n]] | p <- [1..m]] ==>
          bigOr  [pigeon p1 h `And` pigeon p2 h | p1 <- [1..m], p2 <- [1..p1-1], h <- [1..n]]
  where
    pigeon :: Int -> Int -> Frm
    pigeon p h = Var ("P" ++ show (p,h))
```

Then PHP(m,n) will be a tautology just in case m > n, and in particular PHP(n+1,n) is a tautology for all n.

Actually, proving that PHP(n+1,n) is a tautology is a good "stress test" for a SAT solver, and even the best modern solvers are pushed to their limits with relatively small values of of n.
As you can see, our solver already has trouble with PHP(5,4)!

```hs
*Main> :set +s
*Main> isTautology (php 2 1)
True
(0.01 secs, 72,976 bytes)
*Main> isTautology (php 3 2)
True
(0.01 secs, 111,872 bytes)
*Main> isTautology (php 4 3)
True
(0.02 secs, 2,671,744 bytes)
*Main> isTautology (php 5 4)
True
(155.58 secs, 58,949,004,544 bytes)
```

<a name="listmonad"></a>
## Backtracking using the (lazy) list monad

As an alternative to success and failure continuations, we can also program the logic of backtracking using the list monad, modifying the `satisfy` and `falsify` functions to directly return the list of partial satisfying/falsifying assignments, and the functions `isSatisfiable`, `someSolution`, and `allSolutions` appropriately:

```haskell
satisfy',falsify' :: Frm -> Asn -> [Asn]
satisfy' (Var x)   rho = case lookup x rho of
                                    Just v -> if v == True then return rho else []
                                    Nothing -> return ((x,True):rho)
satisfy' (And p q) rho = satisfy' p rho >>= \rho' -> satisfy' q rho'
satisfy' (Or p q)  rho = satisfy' p rho ++ satisfy' q rho
satisfy' (Not p)   rho = falsify' p rho
satisfy' TT        rho = return rho
satisfy' FF        rho = []

falsify' (Var x)   rho = case lookup x rho of 
                                    Just v -> if v == False then return rho else []
                                    Nothing -> return ((x,False):rho)
falsify' (And p q) rho = falsify' p rho ++ falsify' q rho
falsify' (Or p q)  rho = falsify' p rho >>= \rho' -> falsify' q rho'
falsify' (Not p)   rho = satisfy' p rho
falsify' TT        rho = []
falsify' FF        rho = return rho

isSatisfiable' :: Frm -> Bool
isSatisfiable' p = not (null (satisfy' p []))

someSolution' :: Frm -> Maybe Asn
someSolution' p = case satisfy' p [] of
                    []      -> Nothing
                    (rho:_) -> Just rho

allSolutions' :: Frm -> [Asn]
allSolutions' p = satisfy' p []
```

Although the function `isSatisfiable'` is slightly less efficient than its CPS counterpart, it has similar asymptotic complexity due to Haskell's lazy semantics.
In particular, lazy evaluation means that the expression
```hs
not (null (satisfy' p []))
```
will return `True` immediately as soon as the call to `satisfy' p []` returns the first element of the list of satisfying assignments of `p`.
