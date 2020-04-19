Monads in Agda
--------------

We define them ourselves as follows.

Firstly, in the following file

\begin{code}

open import Agda-in-a-Hurry

\end{code}

we define a type Type of "small types" by

  Type = Set

This type is "large".

\begin{code}

LargeType = Set₁

\end{code}

We have that

 Type : LargeType

We don't have Type : Type, which would give a contradiction, similar
to Russell's paradox (https://en.wikipedia.org/wiki/Russell%27s_paradox).

In Haskell the type class of monads is defined by `return` and `>>=`
(pronounced bind), and return and bind are required to satisfy some
equations.

See our adopted book or this link: https://wiki.haskell.org/Monad_laws.

In Agda we can write down these equations in the definition
of monad:

\begin{code}

record Monad (M : Type → Type) : LargeType where

 constructor
  makeMonad

 field
  return : {X : Type} → X → M X

  _>>=_  : {X Y : Type} → M X → (X → M Y) → M Y

  requirement₀ : {X Y : Type} (x : X) (f : X → M Y)
               → (return x >>= f) ≡ f x

  requirement₁ : {X : Type} (m : M X)
               → (m >>= return) ≡ m

  requirement₂ : {X Y Z : Type} (m : M _) (f : X → M Y) (g : Y → M Z)
               → ((m >>= f) >>= g) ≡ (m >>= (λ x → f x >>= g))

\end{code}

Requirements 0 and 1 are known as the unit laws, and requirement 2 is
known as the associativity law.

Example: Not only we define "return" and "_>>=_", but also we show
that they satisfy the requirements:

\begin{code}

Maybe-Monad : Monad Maybe
Maybe-Monad = makeMonad return _>>=_ requirement₀ requirement₁ requirement₂
 where
  return : {X : Type} → X → Maybe X
  return = Just

  _>>=_  : {X Y : Type} → Maybe X → (X → Maybe Y) → Maybe Y
  Nothing >>= f = Nothing
  Just x >>= f = f x

  requirement₀ : {X Y : Type} (x : X) (f : X → Maybe Y)
               → (return x >>= f) ≡ f x
  requirement₀ x f = refl (f x)

  requirement₁ : {X : Type} (m : Maybe X)
               → (m >>= return) ≡ m
  requirement₁ Nothing = refl Nothing
  requirement₁ (Just x) = refl (Just x)

  requirement₂ : {X Y Z : Type} (m : Maybe X) (f : X → Maybe Y) (g : Y → Maybe Z)
               → ((m >>= f) >>= g) ≡ (m >>= (λ x → f x >>= g))
  requirement₂ Nothing g f = refl Nothing
  requirement₂ (Just x) g f = refl (g x >>= f)

\end{code}


For the list monad we need list induction to prove the requirements:

\begin{code}

List-Monad : Monad List
List-Monad = makeMonad return _>>=_ requirement₀ requirement₁ requirement₂
 where
  return : {X : Type} → X → List X
  return = singleton

  _>>=_  : {X Y : Type} → List X → (X → List Y) → List Y
  [] >>= f = []
  (x ∷ xs) >>= f = f x ++ (xs >>= f)

  requirement₀ : {X Y : Type} (x : X) (f : X → List Y)
               → (return x >>= f) ≡ f x
  requirement₀ x f = []-right-neutral (f x)

  requirement₁ : {X : Type} (m : List X)
              → (m >>= return) ≡ m
  requirement₁ [] = refl []
  requirement₁ (x ∷ xs) = ap (cons x) (requirement₁ xs)

  requirement₂ : {X Y Z : Type} (xs : List X) (f : X → List Y) (g : Y → List Z)
               → ((xs >>= f) >>= g) ≡ (xs >>= (λ x → f x >>= g))
  requirement₂ = {!!}
\end{code}

*Bounty.*. The proof of requirement₂ is more difficult, and we leave
it is as a challenge for the interested students. It is by induction
on lists, using the fact that list concatenation is associative.
