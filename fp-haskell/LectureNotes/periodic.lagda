Formal definitions of "periodic" and "eventually periodic" in Agda.

We will not use any library and instead define everything we need from
scratch:

Definition of the type of natural numbers:

\begin{code}

Type = Set

data ℕ : Type where
  zero : ℕ
  succ : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

_+_ : ℕ → ℕ → ℕ
0      + y = y
succ x + y = succ (x + y)

\end{code}

We define "f to the power n" by

(f ^ n) (x) = f(f(f(...f(x))))

where we apply n times the function f.

Formally,

\begin{code}

_^_ : {X : Type} → (X → X) → ℕ → X → X
(f ^ 0)        x = x
(f ^ (succ n)) x = f((f ^ n) x)

\end{code}

Definition of the constructive existential quantifier "Σ" (some people
write "∃", but we won't do this here):

\begin{code}

data Σ {X : Type} (A : X → Type) : Type where
  _,_ : (x : X) → A x → Σ {X} A

\end{code}

To make things easier (and more general as a side-effect), we work
with an arbitrary set of grids, and an arbitrary evolution function
step, which are given as parameters for the following module.

Moreover, instead of equality, we consider a relation between two
grids. In the exercise, the relation is that the two grids become
equal when they are sorted, but here we work with an arbitrary
relation "∼", for the sake of generality.

We use a module with parameters for that:

\begin{code}

module definitions-of-periodicity
         (Grid : Type)
         (_∼_  : Grid → Grid → Type)
         (step : Grid → Grid)
       where

\end{code}

A grid g is periodic if there is k such that applying (k+1) times the
function step to the grid g we get g back:

\begin{code}

 is-periodic : Grid → Type
 is-periodic g = Σ \(k : ℕ) → (step ^ (k + 1)) g ∼ g

\end{code}

A grid g is eventually periodic if there is n such that the grid
(step ^ n) g is periodic:

\begin{code}

 is-eventually-periodic : Grid → Type
 is-eventually-periodic g = Σ \(n : ℕ) → is-periodic ((step ^ n) g)

\end{code}

Notice that we allow n and k to be zero, but we are adding 1 to k to
make sure we work with a positive number.

Recall from the 1st year that in Agda's "constructive logic", truth
values are represented by sets. That's why we have "Type" as the return
type for existential quantification, for the relation ∼, and the
definitions of periodicity and eventual periodicity.

Notice also that what Agda calls sets are often called types.

Although we have kept the relation arbitrary, and this is enough for
the definitions of periodicity, it is normally required that such a
relation is reflexive, transitive and symmetric, or, in other words,
that it is an equivalence relation.
https://en.wikipedia.org/wiki/Equivalence_relation

\begin{code}

 reflexivity symmetry transitivity : Type

 reflexivity  = (g : Grid)     → g ∼ g

 symmetry     = (g h : Grid)   → g ∼ h → h ∼ g

 transitivity = (g h i : Grid) → g ∼ h → h ∼ i → g ∼ i

\end{code}
