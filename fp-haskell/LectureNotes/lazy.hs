f1,f2,f3 :: Integer -> Integer
f1 = \n -> foldr (+) 0 [1..n]
f2 = \n -> foldl (+) 0 [1..n]
f3 = \n -> (n * (n+1)) `div` 2

inc :: Int -> Int
inc n = n + 1

loop :: Int -> Int
loop n = loop (n+1)

const :: a -> b -> a
const x = \y -> x

square :: Int -> Int
square n = n * n

upFrom :: Integer -> [Integer]
upFrom n = n : upFrom (n + 1)

nats,evens,odds :: [Integer]
nats = upFrom 0
evens = map (*2) nats
odds = map (+1) evens

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs

nats',evens',odds' :: [Integer]
evens' = 0 : map (+1) odds'
odds'  = map (+1) evens'
nats'  = interleave evens' odds'

everyOther :: [a] -> [a]
everyOther (x:y:xs) = x:everyOther xs

nats'',evens'',odds'' :: [Integer]
nats''  = [0..]
evens'' = everyOther nats''
odds''  = everyOther (tail nats'')

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

fib' :: [Integer]
fib' = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

data Machine i o = M { out :: o, step :: i -> Machine i o }

run :: Machine i o -> [i] -> o
run m []     = out m
run m (x:xs) = run (step m x) xs

data Bit = B0 | B1  deriving (Show,Eq)

s1,s2 :: Machine Bit Bool
s1 = M { out = True,  step = \b -> case b of B0 -> s2 ; B1 -> s1 }
s2 = M { out = False, step = \b -> case b of B0 -> s1 ; B1 -> s2 }

