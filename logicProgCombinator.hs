{-# LANGUAGE TypeSynonymInstances #-}

module LogicProgCombinator where

factor :: Int -> [(Int, Int)]
factor n = [(r,s) | r <- [1..], s <-[1..], r * s == n]

factor' :: Int -> [(Int, Int)]
factor' n = [(r,s) | (r,s) <- diagprod [1..] [1..], r * s == n]
  where
    diagprod :: [a] -> [b] -> [(a,b)]
    diagprod xs ys = [(xs !! i, ys !! (n-i)) | n <- [0..], i <- [0..n]]


--- 3. Monads for search

newtype Diag a = MkDiag [a] 
  deriving Show

unDiag :: Diag a -> [a]
unDiag (MkDiag xs) = xs

instance Functor Diag where
  fmap f (MkDiag xs) = MkDiag $ fmap f xs

instance Applicative Diag where
  pure x = MkDiag [x]
  f <*> xs = do {f'<-f; x'<-xs; return $ f' x'}

instance Monad Diag where
  return x = MkDiag [x]
  (MkDiag xs) >>= f = MkDiag $ concat $ diag $ map (unDiag . f) xs

diag :: [[a]] -> [[a]]
diag [] = []
diag (xs:xss) = lzw (++) [[x] | x<-xs] ([]: diag xss)

lzw :: (a -> a -> a) -> [a] -> [a] -> [a]
lzw f [] ys = ys
lzw f xs [] = xs
lzw f (x:xs) (y:ys) = (f x y):(lzw f xs ys)


--- 4. Filtering with Conditionals

class Monad m => Bunch m where
  zero  :: m a
  alt   :: m a -> m a -> m a
  wrap  :: m a -> m a

instance Bunch [] where
  zero      = []
  alt xs ys = xs ++ ys
  wrap xs   = xs

instance Bunch Diag where
  zero      = MkDiag []
  alt (MkDiag xs) (MkDiag ys) = MkDiag $ shuffle xs ys
  wrap xs   = xs

shuffle :: [a] -> [a] -> [a]
shuffle [] xs = xs
shuffle (x:xs) ys = x : shuffle ys xs

test :: Bunch m => Bool -> m ()
test b = if b then return () else zero

q1 = do 
  x <- [1..200]
  () <- test (x `mod` 3 == 0)
  return x


--- 5. Breadth-first search

newtype Matrix a = MkMatrix [[a]]
  deriving Show

unMatrix :: Matrix a -> [[a]]
unMatrix (MkMatrix xm) = xm

instance Bunch Matrix where
  zero                        = MkMatrix []
  alt (MkMatrix xm) (MkMatrix ym) = MkMatrix $ lzw (++) xm ym
  wrap (MkMatrix xm)          = MkMatrix $ []:xm


instance Functor Matrix where
  fmap f (MkMatrix xs) = MkMatrix $ fmap (fmap f) xs

instance Applicative Matrix where
  pure x = MkMatrix [[x]]
  f <*> xs = do {f'<-f; x'<-xs; return $ f' x'}

instance Monad Matrix where
  return x = MkMatrix [[x]]
  (MkMatrix xm) >>= f = MkMatrix $ bindM xm $ unMatrix . f


bindM' :: [[t]] -> (t -> [[a]]) -> [[a]]
bindM' xm f = [row n | n <- [0..]]
  where
    row n = [ y | k <- [0..n], x <- xm !! k, y <- (f x) !! (n - k)]

mmm :: [[t]] -> (t -> [[a]]) -> [[[[a]]]]
mmm xm f = map (map f) xm

concatAll :: [[[a]]] -> [[a]]
concatAll = foldr (lzw (++)) []

mmm' :: [[t]] -> (t -> [[a]]) -> [[[a]]]
mmm' xm f = map concatAll $ map (map f) xm

mmm'' :: [[t]] -> (t -> [[a]]) -> [[[a]]]
mmm'' xm f = diag $ map concatAll $ map (map f) xm

mmm''' :: [[t]] -> (t -> [[a]]) -> [[a]]
mmm''' xm f = map concat $ diag $ map concatAll $ map (map f) xm

bindM :: [[t]] -> (t -> [[a]]) -> [[a]]
bindM xm f = map concat $ diag $ map (concatAll . map f) xm


--- 6. Lifting programs to monad level

choose :: Bunch m => [a] -> m a
choose (x:xs) = wrap (return x `alt` choose xs)

factor2 :: Bunch m => Int -> m (Int, Int)
factor2 n = do 
            r <- choose [1..]
            s <- choose [1..]
            test (r*s ==n)
            return (r,s)

run1 = take 10 $ (factor2 24 :: [(Int, Int)])
run2 i = take i $ unMatrix (factor2 24 :: Matrix (Int, Int))


--- 7. terms, substitutions and predicates

data Term = Int Int | Nil | Cons Term Term | Var Variable
  deriving (Show, Eq)

data Variable = Named String | Generated Int
  deriving (Show, Eq)

var :: String -> Term
var s = Var $ Named s
list :: [Int] -> Term
list xs = foldr Cons Nil (map Int xs)

newtype Subst = MkSubst [(Variable, Term)]
  deriving (Show)

unSubst :: Subst -> [(Variable, Term)]
unSubst (MkSubst s) = s

idsubst :: Subst
idsubst = MkSubst []
extend :: Variable -> Term -> Subst -> Subst
extend x t (MkSubst s) = MkSubst $ (x,t) : s

apply :: Subst -> Term -> Term
apply s t = 
  case deref s t of
    Cons x xs -> Cons (apply s x) (apply s xs)
    t' -> t'

deref :: Subst -> Term -> Term 
deref s (Var v) =
  case lookup v (unSubst s) of
    Just t  -> deref s t
    Nothing -> Var v
deref s t = t

unify :: (Term, Term) -> Subst -> Maybe Subst
unify (t,u) s =
  case (deref s t, deref s u) of
    (Nil, Nil)                -> Just s
    (Cons x xs, Cons y ys)    -> unify (x,y) s >>= unify (xs,ys)
    (Int n, Int m) | n == m   -> Just s
    (Var x, Var y) | x == y   -> Just s
    (Var x, t)                -> if occurs x t s 
                                    then Nothing
                                    else Just (extend x t s)
    (t, Var x)                -> if occurs x t s 
                                    then Nothing
                                    else Just (extend x t s)
    (_,_)                     -> Nothing

occurs :: Variable -> Term -> Subst -> Bool
occurs x t s =
  case deref s t of
    Var y     -> x == y
    Cons y ys -> occurs x y s || occurs x ys s
    _         -> False


type Pred m = Answer -> m Answer

newtype Answer = MkAnswer (Subst, Int) --- Int für anzahl der kreierten Vars
  deriving Show

initial :: Answer
initial = MkAnswer (idsubst, 0)

run :: Bunch m => Pred m -> m Answer
run p = p initial

(=:=) :: Bunch m => Term -> Term -> Pred m
(t =:= u) (MkAnswer (s,n)) =
  case unify (t,u) s of
    Just s' -> return $ MkAnswer (s',n)
    Nothing -> zero


query1 = run (var "x" =:= Int 3) :: Matrix Answer 
query2 = run ((var "x" =:= Int 3 )&&& (var "y" =:= Int 4)) :: Matrix Answer 
query3 = run ((var "x" =:= Int 3) &&& (var "x" =:= Int 4)) :: Matrix Answer 

(&&&) :: Bunch m => Pred m -> Pred m -> Pred m
(p &&& q) s = p s >>= q


query4 = run ((var "x" =:= Int 3) ||| (var "x" =:= Int 4)) :: Matrix Answer 

(|||) :: Bunch m => Pred m -> Pred m -> Pred m
(p ||| q) s = alt (p s) (q s)


query5 = run ((var "x" =:= list [1,2,3]) &&& 
              (exists (\t -> var "x" =:= Cons (var "y") t))) :: Matrix Answer
query6 = run ((var "x" =:= list [1,2,3]) &&& 
              (var "x" =:= Cons (var "y") (var "t"))) :: Matrix Answer 
query7 = run (exists (\y -> var "x" =:= Cons y Nil)) :: Matrix Answer 

exists :: Bunch m => (Term -> Pred m) -> Pred m
exists p (MkAnswer (s,n)) = p (Var (Generated n)) (MkAnswer (s,n+1))


query8 = run $ var "x" =:= Int 0 :: Matrix Answer 
query9 = run $ step (var "x" =:= Int 0) :: Matrix Answer 

step :: Bunch m => Pred m -> Pred m
step p s = wrap $ p s

--- 9. recursive programs

append :: Bunch m => (Term, Term, Term) -> Pred m
append (p,q,r) = 
  step (
    ((p =:= Nil) &&& (q =:= r))
    ||| 
    (exists (\x -> exists (\a -> exists (\b ->
      ((p =:= Cons x a) &&& (r =:= Cons x b)) &&& append (a,q,b))))))

--query = (run p) :: [Answer]
query10 = run (append (list[1,2],list[3,4],var "z")) :: Matrix Answer
query11 = run (append (var "x", var "y", list [1,2,3])) :: Matrix Answer

good (s) = 
  step (
    (s =:= Cons (Int 0) Nil)
    ||| 
    (exists (\t -> exists (\q -> exists (\r ->
      ((s =:= Cons (Int 1) t) &&& append (q,r,t)
      &&& good (q) &&& good (r)))))))

query12 = run $ good $ list [1,0,1,1,0,0,1,0,0] :: Matrix Answer
query13 = run $ good $ list [1,0,1,1,0,0,1,0,1] :: Matrix Answer
query14 = run $ good $ var "s" :: Diag Answer
query14 = run $ good $ var "s" :: Matrix Answer
