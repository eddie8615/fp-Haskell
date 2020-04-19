import System.Random

type Lst a = [a]

conj :: Bool -> Bool -> Bool
conj False False = False
conj False True  = False
conj True  False = False
conj True  True  = True

data BW = Black | White

bw2bool :: BW -> Bool
bw2bool Black = False
bw2bool White = True

bool2bw :: Bool -> BW
bool2bw False = Black
bool2bw True  = White

bw2bool' :: BW -> Bool
bw2bool' Black = True
bw2bool' White = False

bool2bw' :: Bool -> BW
bool2bw' False = White
bool2bw' True  = Black

data Bit = Zero | One

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
               deriving (Show, Read, Eq, Ord, Enum)

firstPosition :: Eq a => a -> [a] -> Maybe Integer
firstPosition x []     = Nothing
firstPosition x (y:ys)
           | x == y    = Just 0
           | otherwise = case firstPosition x ys of
                           Nothing -> Nothing
                           Just n  -> Just(n+1)

data And a b = Both a b

data MainDish = Chicken | Pasta | Vegetarian
data Dessert = Cake | IceCream | Fruit
data Drink = Tea | Coffee | Beer

type SaverMenu = Either (And MainDish Dessert) (And MainDish Drink)

type SaverMenu' = And MainDish (Either Dessert Drink)

prime :: SaverMenu -> SaverMenu'
prime (Left (Both m d)) = Both m (Left  d)
prime (Right(Both m d)) = Both m (Right d)

unprime :: SaverMenu' -> SaverMenu
unprime (Both m (Left  d)) = Left (Both m d)
unprime (Both m (Right d)) = Right(Both m d)

and2pair :: And a b -> (a,b)
and2pair (Both x y) = (x,y)

pair2and :: (a,b) -> And a b
pair2and (x,y) = Both x y

type SaverMenu''  = Either (MainDish, Dessert) (MainDish, Drink)
type SaverMenu''' = (MainDish, Either Dessert Drink)

data List a = Nil | Cons a (List a)

nativelist2ourlist :: [a] -> List a
nativelist2ourlist []     = Nil
nativelist2ourlist (x:xs) = Cons x (nativelist2ourlist xs)

ourlist2nativelist :: List a -> [a]
ourlist2nativelist Nil = []
ourlist2nativelist (Cons x xs) = x:ourlist2nativelist xs

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

rev :: List a -> List a
rev Nil         = Nil
rev (Cons x xs) = rev xs `append` (Cons x Nil)

fastrev :: List a -> List a
fastrev xs = revapp xs Nil
  where
    revapp :: List a -> List a -> List a
    revapp (Cons x xs) ys = revapp xs (Cons x ys)
    revapp Nil         ys = ys

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fastfib n = fibAcc n 0 1
  where
    fibAcc 0 x y = x
    fibAcc 1 x y = y
    fibAcc n x y = fibAcc (n-1) y (x+y)

btexample = Fork 8 (Fork 4 (Fork 2 Empty Empty) Empty) (Fork 16 Empty (Fork 20 Empty Empty))

data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)

mirror :: BT a -> BT a
mirror Empty = Empty
mirror (Fork x l r) = Fork x (mirror r) (mirror l)

size :: BT a -> Integer
size Empty        = 0
size (Fork x l r) = 1 + size l + size r

leaves :: BT a -> Integer
leaves Empty        = 1
leaves (Fork x l r) = leaves l + leaves r

height :: BT a -> Integer
height Empty        = 0
height (Fork x l r) =  1 + max (height l) (height r)

btleft = Fork 20 (Fork 16 (Fork 8 (Fork 4 (Fork 2 Empty Empty) Empty) Empty) Empty) Empty

data Direction = L | R deriving (Show)
type Address   = [Direction]

subtree :: Address -> BT a -> Maybe(BT a)
subtree []     t            = Just t
subtree (_:_)  Empty        = Nothing
subtree (L:ds) (Fork _ l _) = subtree ds l
subtree (R:ds) (Fork _ _ r) = subtree ds r

isValid :: Address -> BT a -> Bool
isValid []     _            = True
isValid (_:_)  Empty        = False
isValid (L:ds) (Fork _ l _) = isValid ds l
isValid (R:ds) (Fork _ _ r) = isValid ds r

validAddresses :: BT a -> [Address]
validAddresses Empty        = [[]]
validAddresses (Fork _ l r) = [[]]
                           ++ [L:ds | ds <- validAddresses l]
                           ++ [R:ds | ds <- validAddresses r]

validAddresses' :: BT a -> [Address]
validAddresses' Empty        = [[]]
validAddresses' (Fork _ l r) = [[]]
                            ++ (map (L:) (validAddresses' l))
                            ++ (map (R:) (validAddresses' r))

btpaths :: BT a -> [[a]]
btpaths Empty        = [[]]
btpaths (Fork x l r) = [x:xs | xs <- btpaths l]
                    ++ [x:xs | xs <- btpaths r]

treeInOrder :: BT a -> [a]
treeInOrder Empty = []
treeInOrder (Fork x l r) = treeInOrder l ++ [x] ++ treeInOrder r

treePreOrder :: BT a -> [a]
treePreOrder Empty = []
treePreOrder (Fork x l r) = [x] ++ treePreOrder l ++ treePreOrder r

levels :: BT a -> [[a]]
levels Empty        = []
levels (Fork x l r) = [[x]] ++ zipappend (levels l) (levels r)
  where
    zipappend []       yss      = yss
    zipappend xss      []       = xss
    zipappend (xs:xss) (ys:yss) = (xs ++ ys) : zipappend xss yss

treeBreadthFirst :: BT a -> [a]
treeBreadthFirst = concat . levels

positions :: Eq a => a -> BT a -> [Address]
positions x Empty        = []
positions x (Fork y l r) = [[] | x == y]
                        ++ [L:ds | ds <- positions x l]
                        ++ [R:ds | ds <- positions x r]

elementAt :: Address -> BT a -> a
elementAt []     Empty       = undefined
elementAt []    (Fork x _ _) = x
elementAt (d:ds) Empty       = undefined
elementAt (L:ds)(Fork _ l _) = elementAt ds l
elementAt (R:ds)(Fork _ _ r) = elementAt ds r

balancedTree :: [a] -> BT a
balancedTree [] = Empty
balancedTree xs = let (ys, x:zs) = splitAt (length xs `div` 2) xs in
                  Fork x (balancedTree ys) (balancedTree zs)

balance :: BT a -> BT a
balance = balancedTree . treeInOrder

inOrderTree :: [a] -> [BT a]
inOrderTree [] = [Empty]
inOrderTree xs = [Fork x l r | i <- [0..length xs-1],
                               let (ys, x:zs) = splitAt i xs,
                               l <- inOrderTree ys, r <- inOrderTree zs]

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

isBST' :: Ord a => BT a -> Bool
isBST' t = isIncreasing(treeInOrder t)

isIncreasing :: Ord a => [a] -> Bool
isIncreasing []       = True
isIncreasing (x:[])   = True
isIncreasing (x:y:zs) = x < y && isIncreasing(y:zs)

occurs :: Ord a => a -> BT a -> Bool
occurs x Empty        = False
occurs x (Fork y l r) = x == y
                     || (x < y && occurs x l)
                     || (x > y && occurs x r)

insert :: Ord a => a -> BT a -> BT a
insert x Empty                 = Fork x Empty Empty
insert x (Fork y l r) | x == y = Fork y l r
                      | x <  y = Fork y (insert x l) r
                      | x >  y = Fork y l (insert x r)

insert' :: Ord a => a -> BT a -> Maybe(BT a)
insert' x Empty                = Just(Fork x Empty Empty)
insert' x (Fork y l r)| x == y = Nothing
                      | x <  y = case insert' x l of
                                   Nothing -> Nothing
                                   Just l' -> Just(Fork y l' r)
                      | x >  y = case insert' x r of
                                   Nothing -> Nothing
                                   Just r' -> Just(Fork y l r')

delete :: Ord a => a -> BT a -> BT a
delete x Empty = Empty -- or you may prefer undefined (and even Nothing)
delete x (Fork y l r) | x < y                = Fork y (delete x l) r
                      | x > y                = Fork y l (delete x r)
                      | x == y && l == Empty = r
                      | x == y && r == Empty = l
                      | otherwise            = Fork (largestOf l) (withoutLargest l) r

delete' :: Ord a => a -> BT a -> Maybe (BT a)
delete' x Empty = Nothing
delete' x (Fork y l r) | x < y = case delete' x l of
                                    Nothing -> Nothing
                                    Just l' -> Just (Fork y (delete x l) r)
                       | x > y = case delete' x r of
                                    Nothing -> Nothing
                                    Just r' -> Just (Fork y l (delete x r))
                       | x == y && l == Empty = Just r
                       | x == y && r == Empty = Just l
                       | otherwise = case combinedFunc l of
                                    Nothing -> Nothing
                                    Just l' -> Just (Fork (fst l') (snd l') r)

largestOf :: Ord a => BT a -> a
largestOf Empty            = undefined
largestOf (Fork x l Empty) = x
largestOf (Fork x l r)     = largestOf r

withoutLargest :: Ord a => BT a -> BT a
withoutLargest Empty            = undefined
withoutLargest (Fork x l Empty) = l
withoutLargest (Fork x l r)     = Fork x l (withoutLargest r)

combinedFunc :: Ord a => BT a -> Maybe (a, BT a)
combinedFunc Empty = Nothing
combinedFunc (Fork x l Empty) = Just (x, l)
combinedFunc (Fork x l r) = case combinedFunc r of
                                Nothing -> Nothing
                                Just r' -> Just (fst r', Fork x l r)

randomInts :: [Int]
randomInts = randomRs (minBound,maxBound) (mkStdGen seed)
             where seed = 42

inserts :: Ord a => [a] -> BT a -> BT a
inserts []     t = t
inserts (x:xs) t = inserts xs (insert x t)

aBigBST :: BT Int
aBigBST = inserts (take (10^6) randomInts) Empty

itsHeight = height aBigBST
itsSize   = size aBigBST
itsBST    = isBST aBigBST
itsBST'   = isBST' aBigBST

deletes :: Ord a => [a] -> BT a -> BT a
deletes []     t = t
deletes (x:xs) t = deletes xs (delete x t)

aSmallerTree :: BT Int
aSmallerTree = deletes (take (5 * (10^5)) randomInts) aBigBST

evenBigger :: BT Int
evenBigger = inserts (take (10^7) randomInts) Empty

fullBST :: Integer -> Integer -> BT Integer
fullBST x y | x == y    = Fork x Empty Empty
            | x+1 == y  = Fork y (Fork x Empty Empty) Empty
            | x+1 <  y  = Fork m (fullBST x (m-1)) (fullBST (m+1) y)
            | otherwise = undefined
  where m = (x + y) `div` 2

bstsort :: Ord a => [a] -> [a]
bstsort xs = treeInOrder(inserts xs Empty)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [l | l <- xs, l < x]
            ++ [x]
            ++ qsort [r | r <- xs, r >= x]

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

bigList  = take (10^5) randomInts
hugeList = take (10^6) randomInts

data Rose a = Branch a [Rose a]

rsize :: Rose a -> Integer
rsize (Branch _ ts) = 1 + sum [rsize t | t <- ts]

rsize' :: Rose a -> Integer
rsize' (Branch _ ts) = 1 + sum (map rsize' ts)

rheight :: Rose a -> Integer
rheight (Branch _ []) = 0
rheight (Branch _ ts) = 1 + maximum [rheight t | t <- ts]

rheight' :: Rose a -> Integer
rheight' (Branch _ ts) = 1 + maximum (0 : [rheight' t | t <- ts])

data GameTree board move = Node board [(move, GameTree board move)] deriving (Show)

gameTree :: (board -> [(move,board)]) -> board -> GameTree board move
gameTree plays board = Node board [(m, gameTree plays b) | (m,b) <- plays board]

type NimBoard = [Integer]
data NimMove = Remove Int Integer  deriving (Show,Eq)

nimPlays :: NimBoard -> [(NimMove,NimBoard)]
nimPlays heaps = [(Remove i k, (hs ++ h-k : hs'))
                 | i <- [0..length heaps-1],
                   let (hs, h:hs') = splitAt i heaps,
                   k <- [1..h]]

nim :: [Integer] -> GameTree NimBoard NimMove
nim = gameTree nimPlays

isWinning, isLosing :: Bool -> GameTree board move -> Bool
isWinning isMisere (Node b mgs)
        | null mgs  = isMisere
        | otherwise = any (isLosing isMisere)  [g | (m,g) <- mgs]
isLosing  isMisere (Node b mgs)
        | null mgs  = not (isMisere)
        | otherwise = all (isWinning isMisere) [g | (m,g) <- mgs]

data Tree a = EBranch [(a, Tree a)] deriving (Show)

fullPaths :: Tree a -> [[a]]
fullPaths (EBranch []) = [[]]
fullPaths (EBranch forest) = [x:p | (x,t) <- forest, p <- fullPaths t]

paths :: Tree a -> [[a]]
paths (EBranch forest) =  [] : [x:p | (x,t) <- forest, p <- paths t]

permTree :: Eq a => [a] -> Tree a
permTree xs = EBranch [ (x, permTree(xs \\\ x)) | x <- xs]
  where
    (\\\) :: Eq a => [a] -> a -> [a]
    []     \\\ _   = undefined
    (x:xs) \\\ y
      | x == y     = xs
      | otherwise  = x : (xs \\\ y)

permutations :: Eq a => [a] -> [[a]]
permutations = fullPaths . permTree

factorial n = length(permutations [1..n])

removals, removals2 :: [a] -> [(a,[a])]
removals [] = []
removals (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (removals xs)

type DList a = [a] -> [a]

removals' :: DList a -> [a] -> [(a,[a])]
removals' f [] = []
removals' f (x:xs) = (x, f xs) : removals' (f.(x:)) xs

removals2 = removals' (\xs -> xs)

permTree2 :: [a] -> Tree a
permTree2 xs = EBranch [(y, permTree2 ys) | (y,ys) <- removals2 xs]

permutations2 :: [a] -> [[a]]
permutations2 = fullPaths . permTree2

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

instance Show a => Show(Expr a) where
  show (Value x)       = show x
  show (FromInteger n) = "fromInteger(" ++ show n ++ ")"
  show (Negate e)      = "negate(" ++ show e  ++ ")"
  show (Abs e)         = "abs(" ++ show e ++ ")"
  show (SigNum e)      = "signum(" ++ show e ++ ")"
  show (Add e e')      = "(" ++ show e ++ "+" ++ show e' ++ ")"
  show (Mul e e')      = "(" ++ show e ++ "*" ++ show e' ++ ")"
