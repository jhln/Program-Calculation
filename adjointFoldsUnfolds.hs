{-# LANGUAGE ExistentialQuantification #-}

module AdjointFoldsUnfolds where


-- Adjpint folds and unfolds , Hinze 
----- 3.1 Fixed-Point Equations

type Nat = Int
data Stack = Empty | Push (Nat, Stack)

total :: Stack -> Nat
total Empty = 0
total (Push (n,s)) = n + total s

data Stack' stack = Empty' | Push' (Nat, stack)
instance Functor Stack' where
  fmap f Empty'         = Empty'
  fmap f (Push' (n,s))  = Push' (n, f s)

newtype Myu f = In   {in' :: f (Myu f)}
{-
type Type = Bool | Nat | Expr Type
data Expr :: Type -> * where
  Zero :: Expr Nat
-}

cat :: (Stack, Stack) -> Stack
cat (Empty, ns)       = ns
cat (Push (m,ms), ns) = Push (m, cat (ms,ns))

cat' :: forall x . ((x,Stack) -> Stack)
        -> ((Stack' x, Stack) -> Stack)
cat' cat (Empty', ns) = ns
cat' cat (Push'(m,ms), ns) = Push (m, cat (ms,ns))

catEnd :: (Myu Stack', Stack) -> Stack
catEnd (In ms, ns) = cat' catEnd (ms, ns)