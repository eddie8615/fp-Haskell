merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- The lists of even and odd positions of a list:
eosplit :: [a] -> ([a],[a])
eosplit []       = ([],[])
eosplit [x]      = ([x],[])
eosplit (e:o:xs) = case eosplit xs of
                     (es,os) -> (e:es, o:os)


msort :: Ord a => [a] -> [a]
msort xs | length xs <= 1  =  xs
         | otherwise       = merge (msort es) (msort os)
                             where (es, os) = eosplit xs

-- The above splits the list in a non-standard way. The standard way
-- is as follows:

msort' :: Ord a => [a] -> [a]
msort' xs | length xs <= 1 = xs
          | otherwise      = merge (msort' ys) (msort' zs)
                             where (ys,zs) = splitAt (length xs `div` 2) xs

-- But this goes over the list xs twice, once to compute the length,
-- and then to split, while eosplit goes over the list xs exactly
-- once.

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (a:b:xs) = a <= b && isSorted(b:xs)


-- alternative version using prelude functions
isSorted' :: Ord a => [a] -> Bool
isSorted' xs = all (\i -> xs!!i <= xs!!(i+1)) [0..length xs - 2]
