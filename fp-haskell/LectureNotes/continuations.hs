fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

factcps :: Integer -> (Integer -> r) -> r
factcps 0 k = k 1
factcps n k = factcps (n-1) (\m -> k (n * m))

data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show,Eq)

canopy :: Bin a -> [a]
canopy (L x)   = [x]
canopy (B l r) = canopy l ++ canopy r

canopycps  :: Bin a -> ([a] -> r) -> r
canopycps  (L x)   k = k [x]
canopycps  (B l r) k = canopycps l (\xs -> canopycps r (\ys -> k (xs ++ ys)))

canopycps' :: Bin a -> ([a] -> r) -> r
canopycps' (L x)   k = k [x]
canopycps' (B l r) k = canopycps' r (\ys -> canopycps' l (\xs -> k (xs ++ ys)))

canopycpsIO :: Show a => Bin a -> ([a] -> IO r) -> IO r
canopycpsIO (L x)   k = putStrLn ("visited " ++ show x) >> k [x]
canopycpsIO (B l r) k = canopycpsIO l (\xs -> canopycpsIO r (\ys -> k (xs ++ ys)))

canopycpsIO' :: Show a => Bin a -> ([a] -> IO r) -> IO r
canopycpsIO' (L x)   k = putStrLn ("visited " ++ show x) >> k [x]
canopycpsIO' (B l r) k = canopycpsIO' r (\ys -> canopycpsIO' l (\xs -> k (xs ++ ys)))

canopymon :: Monad m => Bin a -> m [a]
canopymon (L x)   = return [x]
canopymon (B l r) = canopymon l >>= \xs -> canopymon r >>= \ys -> return (xs ++ ys)

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  -- fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f xm = Cont (\k -> runCont xm (k . f))

instance Applicative (Cont r) where
  -- pure :: a -> Cont r a
  pure x = Cont (\k -> k x)
  -- (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  fm <*> xm = Cont (\k -> runCont fm (\f -> runCont xm (\x -> k (f x))))

instance Monad (Cont r) where
  -- return :: a -> Cont r a
  return x = Cont (\k -> k x)
  -- (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  xm >>= g = Cont (\k -> runCont xm (\x -> runCont (g x) k))

abort :: r -> Cont r a
abort x = Cont (\k -> x)

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC xm = Cont (\k -> runCont (xm (\x -> abort (k x))) k)

findcps :: (a -> Bool) -> [a] -> (a -> r) -> r -> r
findcps p []     succ fail = fail
findcps p (x:xs) succ fail
               | p x       = succ x
               | otherwise = findcps p xs succ fail

find1 :: (a -> Bool) -> [a] -> a
find2 :: (a -> Bool) -> [a] -> Maybe a

find1 p xs = findcps p xs id undefined

find2 p xs = findcps p xs Just Nothing

findLeafIndex :: (a -> Bool) -> Bin a -> (Int -> r) -> (Int -> r) -> r
findLeafIndex p (L x) succ fail
                      | p x       = succ 0
                      | otherwise = fail 1
findLeafIndex p (B l r) succ fail = findLeafIndex p l succ
                                                      (\n -> findLeafIndex p r (succ . (n+)) (fail . (n+)))

