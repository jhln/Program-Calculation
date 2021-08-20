{-# LANGUAGE ExistentialQuantification #-}

module AdjunctionsGeneric where

-- Generic Programming with Adjunctions , Hinze 
----- 3.1 Fixed-Point Equations

type Nat = Int 

data DBush = DLeaf Nat | DFork (DBush, DBush) deriving Show

dtotal :: DBush -> Nat
dtotal (DLeaf n) = n
dtotal (DFork (l,r)) = dtotal l + dtotal r

data Tree = Branch (Tree, Nat, Tree) deriving Show

generate :: Nat -> Tree
generate n = Branch (generate (2*n+0), n, generate (2*n+1))

data Bush bush = Leaf Nat | Fork (bush, bush)
instance Functor Bush where
  fmap f (Leaf n)     = Leaf n
  fmap f (Fork(l,r))  = Fork (f l, f r)

newtype Myu f = In   {in' :: f (Myu f)}
newtype Vih f = Out' {out :: f (Vih f)}

ptotal :: Myu Bush -> Nat
ptotal (In (Leaf n)) = n
ptotal (In (Fork (l,r))) = ptotal l + ptotal r

rtotal :: ( Myu Bush -> Nat ) -> Myu Bush -> Nat
rtotal total_f (In (Leaf n)) = n
rtotal total_f (In (Fork (l,r))) = total_f l + total_f r

atotal :: forall x . (x -> Nat) -> (Bush x -> Nat)
atotal total_f (Leaf n) = n
atotal total_f (Fork (l,r)) = total_f l + total_f r

total :: Myu Bush -> Nat
total (In s) = atotal total s



--data Person f = Person { firstName :: f }

----- 3.1.3 Mutual Recursion

--type Vars = Set Char
type Vars = [] Char
data Expr = Var Char | Block (Stat, Expr)
data Stat = Assign (Char, Expr) | Seq (Stat, Stat)

varsExpr :: Expr -> Vars
varsExpr (Var v)        = [v]
varsExpr (Block (s,e))  = varsStat s ++ varsExpr e

varsStat :: Stat -> Vars
varsStat (Assign (x,e)) = x:(varsExpr e)
varsStat (Seq (s1,s2)) = varsStat s1 ++ varsStat s2

newtype Myu1 f1 f2 = In1 {in1' :: f1 (Myu1 f1 f2) (Myu2 f1 f2)}
newtype Myu2 f1 f2 = In2 {in2' :: f2 (Myu1 f1 f2) (Myu2 f1 f2)}

data Expr' expr stat = Var' Char | Block' (stat,expr)
data Stat' expr stat = Assign' (Char, expr) | Seq' (stat,stat)

varsExpr' :: forall x1 x2 . (x1 -> Vars, x2 -> Vars)
             -> (Expr' x1 x2 -> Vars)
varsExpr' (varsExpr, varsStat) (Var' v) = [v]
varsExpr' (varsExpr, varsStat) (Block' (s,e))
  = varsStat s ++ varsExpr e

varsStat' :: forall x1 x2 . (x1 -> Vars, x2 -> Vars)
             -> (Stat' x1 x2 -> Vars)
varsStat' (varsExpr, varsStat) (Assign' (x,e)) = x:varsExpr e
varsStat' (varsExpr, varsStat) (Seq' (s1,s2))
  = varsStat s1 ++ varsStat s2

varsExprEnd :: Myu1 Expr' Stat' -> Vars
varsExprEnd (In1 e) = varsExpr' (varsExprEnd, varsStatEnd) e
varsStatEnd :: Myu2 Expr' Stat' -> Vars
varsStatEnd (In2 s) = varsStat' (varsExprEnd, varsStatEnd) s

----- 3.1.4 Type Functors


----- 3.2. Adjoint Fixed-Point Equation

data Stack = Empty | Push (Nat, Stack)

cat :: (Stack, Stack) -> Stack
cat (Empty, ns)       = ns
cat (Push (m,ms), ns) = Push (m, cat (ms,ns))

----- 3.3. Exploring Adjunctions

----- 3.3.1 Currying

data Stack' stack = Empty' | Push' (Nat, stack)
instance Functor Stack' where
  fmap f Empty'         = Empty'
  fmap f (Push' (n,s))  = Push' (n, f s)

type L x = ( x, Stack )

cat' :: forall x . (L x -> Stack) 
        -> (L (Stack' x) -> Stack)
cat' cat (Empty', ns) = ns
cat' cat (Push' (m,ms), ns) = Push (m, cat (ms, ns))


catEnd :: L (Myu Stack') -> Stack
catEnd (In ms, ns) = cat' catEnd (ms, ns)


{-
cat' :: forall x . ((x,Stack) -> Stack)
        -> ((Stack' x, Stack) -> Stack)
cat' cat (Empty', ns) = ns
cat' cat (Push'(m,ms), ns) = Push (m, cat (ms,ns))

catEnd :: (Myu Stack', Stack) -> Stack
catEnd (In ms, ns) = cat' catEnd (ms, ns)
-}
----- 3.3.2 Mutual Value Recursion

----- 3.3.3 Single Value Recursion


