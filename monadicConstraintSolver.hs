{-# LANGUAGE TypeFamilies #-}

module MonadicConstraintSolver where

class Monad solver => Solver solver where
  type Constraint solver :: *
  type Term solver :: *
  newvar :: solver (Term solver)
  add :: Constraint solver -> solver Bool
  run :: solver a -> a
  type Label solver :: *
  mark :: solver (Label solver)
  goto :: Label solver -> solver ()

date FD a = FD a

instance Solver FD where
  type Constraint FD = FDConstraint
  type Term FD = FDTerm
  newvar = newvarFD
  add = addFD
  run = runFD
  

newvarFD = undefined
addFD = undefined
runFD = undefined

data FDConstraint 
  = FDIn FDTerm (Int,Int)
  | FDEQ FDTerm Int
  | FDNE FDTerm FDTerm Int

data FDTerm = Var Variable -- Int Int | Nil | Cons Term Term 
  deriving (Show, Eq)

data Variable = Named String | Generated Int
  deriving (Show, Eq)

data Tree solver a
  = Return a
  | NewVar (Term solver -> Tree solver a)
  | Add (Constraint solver) (Tree solver a)
  | Try (Tree solver a) (Tree solver a)
  | Fail
  | Dynamic (solver (Tree solver a))

instance Monad (Tree solver) where
  return = Return
  (>>=) = bind

bind :: Tree solver a -> (a -> Tree solver b) -> Tree solver b
(Return x) `bind` k = k x
(NewVar f) `bind` k = NewVar (\v -> f v `bind` k)
(Add c t) `bind` k = Add c (t `bind` k)
Fail `bind` k = Fail
(Try l r) `bind` k = Try (l `bind` k) (r `bind` k)


true = Return ()
false = Fail
t1 /\ t2 = t1 >>= \_ -> t2
t1 \/ t2 = Try t1 t2
conj = foldr (/\) true
disj = foldr (\/) false
exists = NewVar
exist n f = aux n []
where aux 0 vs = f $ reverse vs
aux n vs = exists $ \v -> aux (n-1) (v:vs)



solve :: Solver solver => Tree solver a -> a
solve = run . eval

{-
eval :: Solver solver => Tree solver a -> solver a
eval (Return x) = return x
eval (Add c t) = add c >> eval t
eval (NewVar f) = newvar >>= \v -> eval (f v)
-}

eval :: Solver solver => Tree solver a -> solver [a]
eval model = eval’ model []

eval’ (Return x) wl = do 
                        xs <- continue wl
                        return (x:xs)
eval’ (Add c t) wl = do 
                      b <- add c
                      if b 
                        then eval’ t wl 
                        else continue wl
eval’ (NewVar f) wl = do 
                        v <- newvar 
                        eval’ (f v) wl
eval’ (Try l r) wl = do 
                      now <- mark
                      eval’ l ((now,r):wl)
eval’ Fail wl = continue wl
eval’ (Dynamic m) wl = do 
                        t <- m
                        eval’ t wl

continue [] = return []
continue ((past,t):wl) = do 
                          goto past
                          eval’ t wl





----- Queens Problem

nqueens n = exist n $ \queens -> model queens n /\ enumerate queens [1..n]
enumerate qs values = conj [ enum q values | q <- qs ]
enum var values = disj [ var @= value | value <- values ]

disj = foldl (\/) false
(\/) = Try
fasle = Fail