module Exercises where

data Nat = Zero | Succ Nat  deriving (Show, Eq)
data T = Nil | Cons (A,T)   deriving (Show, Eq)
type A = Int

p1 :: (a, b) -> a
p1 = fst 
p2 :: (a, b) -> b
p2 = snd 

(<<>>) :: ( a -> b ) -> ( a -> c ) -> a -> (b,c)
(f <<>> g) x = (f x, g x)  

(<&>) :: (a->b) -> (c->d) -> (a,c) -> (b,d)
f <&> g = (f . p1) <<>> (g . p2)

(!) :: a -> ()
(!) = const ()

i1 :: a -> Either a b
i1 = Left
i2 :: b -> Either a b
i2 = Right

(<|>) :: (a->c) -> (b->c) -> Either a b -> c
(f <|> g) (Left x) = f x
(f <|> g) (Right x) = g x

(<+>) :: (a->b) -> (c->d) -> Either a c -> Either b d
f <+> g = (i1 . f) <|> (i2 . g)

(?) :: (a -> Bool) -> a -> Either a a
p? x = if p x 
        then i1 x
        else i2 x


for :: (t -> t) -> t -> Nat -> t
for f i Zero = i
for f i (Succ n) = f $ for f i n

outNat :: Nat -> Either () Nat
outNat Zero = i1 ()
outNat (Succ n) = i2 n

f :: Nat -> Nat
f = for Succ Zero

outT :: T -> Either () (A,T)
outT Nil = i1 ()
outT (Cons (a,l)) = i2 (a,l)

outT2 :: T -> Either () (A,T)
outT2 = ((!) <+> (hd <<>> tl)) . ((== Nil)?)

hd :: T -> A
hd (Cons (a,t)) = a
hd Nil = error "not implemented"

tl :: T -> T
tl (Cons (a,t)) = t 
tl Nil = error "not implemented"


add :: Num a => (a, a) -> a
add (x,y) = x + y

