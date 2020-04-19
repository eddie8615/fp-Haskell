infixl 6 --->
(--->) :: a -> (a -> b) -> b
x ---> f  =  f x

infixl 6 -->

(-->) :: Maybe a -> (a -> Maybe b) -> Maybe b
xm --> f = case xm of
             Nothing -> Nothing
             Just x  -> f x

f 0 = 1
f 1 = 1
f n = f(n-2) + f(n-1)

mfib :: (Eq a, Num a, Monad m) => a -> m a
mfib n =  if n == 0 || n == 1
          then return 1
          else do
                x <- mfib(n-1)
                y <- mfib(n-2)
                return(x + y)

data Id a = Return a
            deriving (Show)

instance Functor Id where
  fmap f (Return x) = Return(f x)

instance Applicative Id where
  pure = Return
  (Return f) <*> (Return x) = Return(f x)

instance Monad Id where
  (Return x) >>= f = f x

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

data State s a = T(s -> (a,s))

runState :: State s a -> (s -> (a, s))
runState (T p) = p

instance Monad (State s) where
  return x = T(\u -> (x,u))
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  xm >>= f = case xm of
               T p -> T(\u -> case p u of
                                (x, v) -> case f x of
                                            T q -> q v)

instance Functor(State s) where
  fmap f xm = xm >>= return . f

instance Applicative(State s) where
      pure = return
      xm <*> ym = xm >>= \x -> ym >>= return . x

get :: State s s
get = T(\s -> (s,s))

put :: s -> State s ()
put s = T (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = T(\s -> ((), f s))

sfib :: (Enum s, Eq a, Num a, Num b) => a -> State s b
sfib n = do
          modify succ -- this is the only line changed
          if n == 0 || n == 1
          then return 1
          else do
                x <- sfib(n-1)
                y <- sfib(n-2)
                return(x + y)

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

ifib :: (Eq a, Num a, Num b) => a -> State (b, b) b

ifib 0 = do
          (x , y) <- get
          return x

ifib n = do
          (x , y) <- get
          put (y, x+y)
          ifib (n-1)

imperativefib :: (Eq a, Num a) => a -> a
imperativefib n = fst(runState (ifib n) (1 , 1))

iofib :: (Show a, Eq a, Num a, Num b) => a -> IO b
iofib n = do
           putStrLn(show n) -- this is the only line changed
           if n == 0 || n == 1
           then return 1
           else do
                 x <- iofib(n-1)
                 y <- iofib(n-2)
                 return(x + y)

data T a = Leaf a | Branch (T a) (T a)

instance Functor T where
  -- fmap :: (a -> b) -> T a -> T b
  fmap f (Leaf a)     = Leaf(f a)
  fmap f (Branch t s) = Branch (fmap f t) (fmap f s)

tcollapse :: T(T a) -> T a
tcollapse (Leaf t)     = t
tcollapse (Branch t s) = Branch (tcollapse t) (tcollapse s)

instance Monad T where
  -- return :: a -> T a
  return = Leaf

  -- (>>=) :: T a -> (a -> T b) -> T b
  t >>= f = tcollapse(fmap f t)

instance Applicative T where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

(>>>=) :: T a -> (a -> T b) -> T b
(Leaf x)     >>>= f = f x
(Branch t s) >>>= f = Branch (t >>>= f) (s >>>= f)

tcollapse' ::  T(T a) -> T a
tcollapse' tt = tt >>>= (\t -> t >>>= return)

