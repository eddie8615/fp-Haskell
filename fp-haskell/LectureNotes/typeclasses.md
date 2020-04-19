# Type classes and instances

These notes should be read in conjunction with chapter 8.5 of our textbook Programming in Haskell, as well as [Chapter 6 of Real World Haskell](http://book.realworldhaskell.org/read/using-typeclasses.html).

* We discuss some examples from the [Haskell'98 standard prelude](https://www.haskell.org/onlinereport/standard-prelude.html) for pedagogical purposes.

* See the [prelude for the current version of the language](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html) for all predefined classes and their instances.

## Classes and instances

* A `class` in Haskell is like an interface in Java.

* We implement a class in Haskell using the keyword `instance`.

* Only types that are introduced using `data` or `newtype` can be made instances of classes (although GHC has some extensions to get around this...).

* It is only possible to declare a *single* instance of a class for any given `data` type or `newtype` (although GHC has some extensions to get around this...).

## Equality class
```hs
class  Eq a  where
    (==), (/=) :: a -> a -> Bool

        -- Minimal complete definition:
        --      (==) or (/=)
    x /= y     =  not (x == y)
    x == y     =  not (x /= y)
```
When giving an instance of this class, we may define `==` only (in which case
`/=` is defined using the above default definition) or `/=` only (in which case `==` is defined using the above default definition), or both (in which case the default definitions are ignored). But we must define at least one of them.

For example, the following defines an instance of the `Eq` class to represent modular arithmetic:
```haskell
newtype IntMod = IntMod (Int,Int)

instance Eq IntMod where
  IntMod (k1,n1) == IntMod (k2,n2) = n1 == n2 && k1 `mod` n1 == k2 `mod` n2
```

After we declare the instance, polymorphic functions referring to the `Eq` class can be used automatically, for example the `elem :: Eq a => a -> [a] -> Bool` function:
```hs
> elem (IntMod (7,5)) [IntMod (1,5), IntMod (2,5), IntMod (3,5)]
True
```

Note that Haskell only requires instance declarations to be well-typed, not that they provide the "expected" semantics of the operations.
For example, nothing prevents us from declaring `IntMod` to be an `Eq` instance as follows (other than the fact that we already declared the instance above, and we are only allowed one instance of a class per type):
```hs
instance Eq IntMod where
  IntMod (k1,n1) == IntMod (k2,n2) = False
  IntMod (k1,n1) /= IntMod (k2,n2) = False
```

## Ordering class

This relies on the following definition, where `LT` stands for *less than*, `EQ` stands for *equal*, and `GT` stands for *greater than*:
```hs
data  Ordering  =  LT | EQ | GT
          deriving (Eq, Ord, Enum, Read, Show, Bounded)
```
There is a seeming circularity between the definition of `Ordering` above and `Ord` below.
The following defines the `Ord` class for types `a` which are in the `Eq` class.
```hs
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

        -- Minimal complete definition:
        --      (<=) or compare
        -- Using compare can be more efficient for complex types.
    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y
         | x <= y    =  y
         | otherwise =  x
    min x y
         | x <= y    =  x
         | otherwise =  y
```

For example, lists are declared as an instance of `Ord` as follows:

```hs
instance (Ord a) => Ord [a] where
  compare [] []         = EQ
  compare [] (_:_)      = LT
  compare (_:_) []      = GT
  compare (x:xs) (y:ys) = case compare x y of
                              EQ    -> compare xs ys
                              other -> other
```

## Enumeration class
```hs
class  Enum a  where
    succ, pred       :: a -> a
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

        -- Minimal complete definition:
        --      toEnum, fromEnum
--
-- NOTE: these default methods only make sense for types
--   that map injectively into Int using fromEnum
--  and toEnum.
    succ             =  toEnum . (+1) . fromEnum
    pred             =  toEnum . (subtract 1) . fromEnum
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]
    enumFromThenTo x y z =
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]
```

## Bounded class

For types that have a smallest and a largest element:
```hs
class  Bounded a  where
    minBound         :: a
    maxBound         :: a
```
## Numeric classes

### Num

```hs
class  (Eq a, Show a) => Num a  where
    (+), (-), (*)    :: a -> a -> a
    negate           :: a -> a
    abs, signum      :: a -> a
    fromInteger      :: Integer -> a

        -- Minimal complete definition:
        --      All, except negate or (-)
    x - y            =  x + negate y
    negate x         =  0 - x
```

### Real
```hs
class  (Num a, Ord a) => Real a  where
    toRational       ::  a -> Rational
```

### Integral
```hs
class  (Real a, Enum a) => Integral a  where
    quot, rem        :: a -> a -> a
    div, mod         :: a -> a -> a
    quotRem, divMod  :: a -> a -> (a,a)
    toInteger        :: a -> Integer

        -- Minimal complete definition:
        --      quotRem, toInteger
    n `quot` d       =  q  where (q,r) = quotRem n d
    n `rem` d        =  r  where (q,r) = quotRem n d
    n `div` d        =  q  where (q,r) = divMod n d
    n `mod` d        =  r  where (q,r) = divMod n d
    divMod n d       =  if signum r == - signum d then (q-1, r+d) else qr
                        where qr@(q,r) = quotRem n d
```
### Fractional
```hs
class  (Num a) => Fractional a  where
    (/)              :: a -> a -> a
    recip            :: a -> a
    fromRational     :: Rational -> a

        -- Minimal complete definition:
        --      fromRational and (recip or (/))
    recip x          =  1 / x
    x / y            =  x * recip y
```

### Floating
```hs
class  (Fractional a) => Floating a  where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

        -- Minimal complete definition:
        --      pi, exp, log, sin, cos, sinh, cosh
        --      asin, acos, atan
        --      asinh, acosh, atanh
    x ** y           =  exp (log x * y)
    logBase x y      =  log y / log x
    sqrt x           =  x ** 0.5
    tan  x           =  sin  x / cos  x
    tanh x           =  sinh x / cosh x
```

### RealFrac
```hs
class  (Real a, Fractional a) => RealFrac a  where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

        -- Minimal complete definition:
        --      properFraction
    truncate x       =  m  where (m,_) = properFraction x

    round x          =  let (n,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                          in case signum (abs r - 0.5) of
                                -1 -> n
                                0  -> if even n then n else m
                                1  -> m

    ceiling x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFraction x

    floor x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFraction x
```

### RealFloat
```hs
class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                     :: a -> Bool
    atan2            :: a -> a -> a

        -- Minimal complete definition:
        --      All except exponent, significand,
        --                 scaleFloat, atan2
    exponent x       =  if m == 0 then 0 else n + floatDigits x
                        where (m,n) = decodeFloat x

    significand x    =  encodeFloat m (- floatDigits x)
                        where (m,_) = decodeFloat x

    scaleFloat k x   =  encodeFloat m (n+k)
                        where (m,n) = decodeFloat x

    atan2 y x
      | x>0           =  atan (y/x)
      | x==0 && y>0   =  pi/2
      | x<0  && y>0   =  pi + atan (y/x)
      |(x<=0 && y<0)  ||
       (x<0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                      = -atan2 (-y) x
      | y==0 && (x<0 || isNegativeZero x)
                      =  pi    -- must be after the previous test on zero y
      | x==0 && y==0  =  y     -- must be after the other double zero tests
      | otherwise     =  x + y -- x or y is a NaN, return a NaN (via +)
```
