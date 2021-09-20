module MonadicConstraintOverview where

data Tree solver a
  = Return a
  | NewVar (Term solver -> Tree solver a)
  | Add (Constraint solver) (Tree solver a)
  | Try (Tree solver a) (Tree solver a)
  | Fail
  | Dynamic (solver (Tree solver a))

instance Monad (Tree solver) where
  return = Return
  (Return x) >>= f = f x
  (NewVar g) >>= f = NewVar (\v -> g x >>= f)
  (Add c t) >>= f = Add c (t >>= f)
  (Try t1 t2) >>= f = Try (t1 >>= f) (t2 >>= f)
  Fail >>= f = Fail
  Dynamic m >>= f = Dynamic (do { t <- m ; return (t >>= f)})

class Monad solver => Solver solver where
  type Constraint solver :: *
  type Term solver :: *
  newvar :: solver (Term solver)
  add :: Constraint solver -> solver Bool
  run :: solver a -> a
  type Label solver :: *
  mark :: solver (Label solver)
  goto :: Label solver -> solver ()


true = Return ()
false = Fail

t1 /\ t2 = t1 >>= \_ -> t2
t1 \/ t2 = Try t1 t2

conj = foldr (/\) true
disj = foldr (\/) false

exists = NewVar

exist n f = aux n []
  where 
    aux 0 vs = f $ reverse vs
    aux n vs = exists $ \v -> aux (n-1) (v:vs)

