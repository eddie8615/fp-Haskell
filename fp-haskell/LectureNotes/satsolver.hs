type Atm = String
data Frm = Var Atm | And Frm Frm | Or Frm Frm | Not Frm | TT | FF
     deriving (Show,Eq)

(==>) :: Frm -> Frm -> Frm
(==>) p q = Not p `Or` q

bigAnd, bigOr :: [Frm] -> Frm
bigAnd = foldr And TT
bigOr  = foldr Or  FF

type Asn = [(Atm,Bool)]

evalFrm :: Frm -> Asn -> Bool
evalFrm (Var x)   rho = case lookup x rho of
                          Just v  -> v
                          Nothing -> False      -- could also return True here, or raise an error
evalFrm (And p q) rho = evalFrm p rho && evalFrm q rho
evalFrm (Or p q)  rho = evalFrm p rho || evalFrm q rho
evalFrm (Not p)   rho = not (evalFrm p rho)

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

isSatisfiable p = satisfy p [] (\_ _ -> True) False

someSolution p = satisfy p [] (\s _ -> Just s) Nothing

allSolutions p = satisfy p [] (:) []

kcolor :: Int -> [Int] -> [(Int,Int)] -> Frm
kcolor k verts edges =
   bigAnd [bigOr [hascolour x c | c <- [1..k]] | x <- verts] `And`
   bigAnd [Not (hascolour x c `And` hascolour y c) | (x,y) <- edges, c <- [1..k]]
   where
     hascolour x c = Var ("C" ++ show (x,c))

prismGraph = kcolor 3 [1..6] [(1,2),(1,3),(1,4),(2,3),(2,6),(3,5),(4,5),(4,6),(5,6)]

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

isTautology :: Frm -> Bool
isTautology = not . isSatisfiable . Not

php :: Int -> Int -> Frm
php m n = bigAnd [bigOr [pigeon p h | h <- [1..n]] | p <- [1..m]] ==>
          bigOr  [pigeon p1 h `And` pigeon p2 h | p1 <- [1..m], p2 <- [1..p1-1], h <- [1..n]]
  where
    pigeon :: Int -> Int -> Frm
    pigeon p h = Var ("P" ++ show (p,h))

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

