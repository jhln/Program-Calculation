module Exercises where

data Nat = Zero | Succ Nat  deriving (Show, Eq)
data T = Nil | Cons (Nat,T)   deriving (Show, Eq)
type A = Nat

-- function  app page 20
-- TODO (2.7) quickcheck für def
-- TODO (2.8) quickcheck für assoc

(#) :: (b -> c) -> (a -> b) -> (a -> c)
f # g = \x -> f (g x)

-- id page 21
-- TODO (2.10) quickcheck für commutativ
(§) :: a -> a
(§) x = x

-- const  page 22
-- TODO (2.15) quickcheck für commutativ
(°°) :: a -> b -> a
a °° _ = a

{- inverse sign
(°) :: (a -> b) -> b -> a
(°) f = let new =  
-}

----------------------------------------------------------------------------------------------

-- split
-- TODO (2.26) quickcheck für x-fusion
(<&>) :: ( a -> b ) -> ( a -> c ) -> a -> (b,c)
(f <&> g) x = (f x, g x)


-- projections
-- TODO (2.21) quickcheck für def
-- TODO (2.22) quickcheck für x-cancellation
pi1 :: (a, b) -> a
pi1 = fst
pi2 :: (a, b) -> b
pi2 = snd

-- product
-- TODO (2.24) quickcheck für def
(><) :: (a->b) -> (c->d) -> (a,c) -> (b,d)
f >< g = (f . pi1) <&> (g . pi2)

-- TODO (2.27) quickcheck für x-absorbtion
-- TODO (2.28/29) quickcheck für products and projections
-- TODO (2.30) quickcheck für x-functor
-- TODO (2.31) quickcheck für x-id
-- TODO (2.32) quickcheck für x-reflection

-- x_swap
-- TODO (2.30) quickcheck für x_swap # x_swap = §
x_swap :: (a,b) -> (b,a)
x_swap = pi2 <&> pi1
      -- let (e1,e2) = f x
      --      in (e2,e1)

-- x_assocr
-- TODO (2.35) quickcheck für iso ((A,B),C) ~ (A,(B,C))
x_assocr :: ((a,b),c) -> (a,(b,c))
x_assocr = (pi1 # pi1) <&> (pi2 >< (§))

-- x_assocl
-- for 2.17
x_assocl :: (a,(b,c)) -> ((a,b),c)
x_assocl = ((§) >< pi1) <&> (pi2 # pi2)

-----------------------------------------------------------------------------------------------------

-- tagiig page 31
in1 :: a -> Either a b
in1 = Left
in2 :: b -> Either a b
in2 = Right

-- either
-- TODO (2.38) quickcheck for def
-- TODO (2.40) quickcheck for +-cancellation
-- TODO (2.42) quickcheck for +-fusion
(<+>) :: (a->c) -> (b->c) -> Either a b -> c
(f <+> g) (Left x) = f x
(f <+> g) (Right x) = g x

-- coproduct
-- TODO (2.39) quickcheck for def
(-|-) :: (a->b) -> (c->d) -> Either a c -> Either b d
f -|- g = (in1 # f) <+> (in2 # g)

-- TODO (2.43) quickcheck for +-absorbtion
-- TODO quickcheck for coproducts and injections
-- TODO (2.44) quickcheck for +-functor
-- TODO (2.45) quickcheck for +-id
-- TODO (2.41) quickcheck for +-reflection


-- exercise 2.5
-- 't' because of similar shape to '+'
-- TODO quickcheck for t_swap # t_swap = §
t_swap :: Either a b -> Either b a
t_swap (Left a) = Right a
t_swap (Right a) = Left a


-- exercise 2.6
-- 't' because of similar shape to '+'
-- TODO (2,48) quickcheck for def
t_assocr :: (Either (Either a b) c) -> Either a (Either b c)
t_assocr = ((§) -|- in1) <+> (in2 # in2)


-- TODO exercise 2.10
-- show ((f <+> h) # (pi1 -|- pi1 )) <&> ((g <&> k) # (pi2 -|- pi2))
-- reduces to (f -|- g) <&> (h -|- k)
-- ((f <+> h) # (pi1 -|- pi1 )) <&> ((g <&> k) # (pi2 -|- pi2))
-- by +-absorption
-- ((f # pi1) <+> (h # pi1)) <&> ((g # pi2) <&> (k # pi2))
-- by exchange law
-- ((f # pi1) <&> (g # pi2)) <+> (h # pi1 <&> k # pi2))
-- by coproduct-def
-- (f -|- g) <&> (h -|- k)

-------------------------------------------------------------------------------------

-- exchange-law in both directions 7: -->, 4: <--
-- TODO (2.49) quickcheck for exchange law
exchangelaw__7 :: Either (f,g) (h,k) -> (Either f h, Either g k)
exchangelaw__7 =  (in1 >< in1) <+> (in2 >< in2)
-- excahge law other direction is unclear
-- not clear how to handle (f,k) or (h,g)
--exchangelaw__4 :: (Either f h, Either g k) -> Either (f,g) (h,k) 
--exchangelaw__4 =  (in1 <+> in1) <&> (in2 <+> in2)

exLawLeft :: (a->b) -> (a->d) -> (c->b) -> (c->d) -> Either a c -> (b,d)
exLawLeft f g h k = (f <&> g) <+> (h <&> k)
exLawRight :: (a->b) -> (a->d) -> (c->b) -> (c->d) -> Either a c -> (b,d)
exLawRight f g h k = (f <+> h) <&> (g <+> k)


-- undistr
-- TODO (2.52) quickcheck for def
-- TODO exercise 2.7 apply exchange law !?!?!?!
undistr :: Either (a,b) (a,c) -> (a,Either b c)
undistr =  ((§) >< in1) <+> ((§) >< in2)



-- exercise 2.11
-- undistl
undistl :: Either (b,a) (c,a) -> (Either b c, a)
undistl =  x_swap # undistr # (x_swap -|- x_swap)

-- TODO exercies 2.12
-- TODo exercise 2.13


pattern_f :: (a->d) -> (b->e) -> (c->f) -> (a, Either b c) -> (d, Either e f)
pattern_f f g h = f >< (g -|- h)
pattern_g :: (a->d) -> (b->e) -> (c->f) -> Either (a,b) (a,c) -> Either (d,e) (d,f)
pattern_g f g h = (f >< g) -|- (f >< h)


-------------------------------------------------------------------

-- Natrual Properties

-- TODO ex. 2.14
-- TODO ex. 2.15
-- TODO ex. 2.16


-------------------------------------------------------------------

-- Universal Properties

-- TODO ex. 2.17
-- with x_assocr and x_assocl fromabove

-- show: x_assocr # assocl = id
-- (pi1 # pi1) <&> (pi2 >< (§)) # assocl
-- by x-fusion
-- (pi1 # pi1 # assocl) <&> ((pi2 >< (§)) # assocl)
-- by x-cancellation and x-absorption 
-- (pi1 # (§)) <&> ((pi2 # ((§) >< pi1)) >< ((§) # (pi2 # pi2)))
-- by x-cancellation
-- pi1 <&> ((pi1 # pi2) >< (pi2 # pi2)) = id_(a,(b,c))

-- TODO ex. 2.18

leftSide f k g = (f >< (k°°)) -|- (g >< (k°°)) 
-- by universal properties:

-- leftSide # in1 = f >< (k°°)
-- pi1 # leftSide # in1 = f
-- pi2 # leftSide # in1 = k°°

-- leftSide # in2 = g >< (k°°)
-- pi1 # leftSide # in2 = g
-- pi2 # leftSide # in2 = k°°


rightSide f k g = (f -|- g) >< (k°°)
-- by universal properties:

-- pi1 # rightSide = f -|- g
-- pi1 # rightSide # in1 = f , thus same as above
-- pi1 # rightSide # in2 = g , thus same as above

-- pi2 # rightSide = k°° because auf +-id is the same as:
-- pi2 # leftSide # in1 = k°° and pi2 # leftSide # in2 = k°°


-- TODO ex. 2.19

-- +-reflection
-- id = f -|- g
-- by universal properties
-- id # in1 = f thus in1 = f
-- id # in2 = g thus in2 = g
-- thus id = in1 -|- in2

-- TODO ex. 2.20
-- TODO ex. 2.21

-------------------------------------------------------------------


(!) :: a -> ()
(!) = const ()


(?) :: (a -> Bool) -> a -> Either a a
p? x = if p x
        then in1 x
        else in2 x


for :: (t -> t) -> t -> Nat -> t
for f i Zero = i
for f i (Succ n) = f $ for f i n

outNat :: Nat -> Either () Nat
outNat Zero = in1 ()
outNat (Succ n) = in2 n

f :: Nat -> Nat
f = for Succ Zero

outT :: T -> Either () (A,T)
outT Nil = in1 ()
outT (Cons (a,l)) = in2 (a,l)

outT2 :: T -> Either () (A,T)
outT2 = ((!) -|- (hd <&> tl)) . ((== Nil)?)

hd :: T -> A
hd (Cons (a,t)) = a
hd Nil = error "not implemented"

tl :: T -> T
tl (Cons (a,t)) = t
tl Nil = error "not implemented"


add :: (Nat, Nat) -> Nat
add (x,Zero) = x
add (x, Succ n) = Succ $ add (x,n)

synthyTest :: T
synthyTest = synthy $ Succ $ Succ $ Succ $ Succ $ Zero
synthy2Test :: T
synthy2Test = synthy2 $ Succ $ Succ $ Succ $ Succ $ Zero
synthyTestAnswer :: T
synthyTestAnswer = Cons (Succ (Succ (Succ (Succ Zero))),Cons (Succ (Succ (Succ Zero)),Cons (Succ (Succ Zero),Cons (Succ Zero,Nil))))


synthy2 :: Nat -> T
synthy2 = (const Nil <+> (Cons . (id >< synthy))) . ((!) -|- (id <&> pre)) . ((== Zero)?)

-- page 83
synthy :: Nat -> T
synthy =  (const Nil <+> (Cons . (id <&> (synthy . pre)) ) ) . ((== Zero)?)

pre :: Nat -> Nat
pre (Succ n) = n
pre Zero = Zero

addListTest :: Nat
addListTest = addList synthyTest
addListTestAnswer :: Nat
addListTestAnswer = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))



-- page 82
addList :: T -> Nat
addList = ( const Zero <+> (add . ( hd <&> (addList . tl) )) )
          . ((== Nil)?)


-- Ex 3.7

-- page 83
synthyOdd :: Nat -> T
synthyOdd =  (const Nil <+> (Cons . ((pre . add . (id <&> id)) <&> (synthyOdd . pre)) ) ) . ((== Zero)?)

sq :: Nat -> Nat
sq = addList . synthyOdd

sqTest :: Nat
sqTest = sq $ Succ $ Succ $ Succ Zero
sqTestAnswer :: Nat
sqTestAnswer = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))

-- page 86

fact :: Nat -> Nat
fact =  ( const (Succ Zero) <+>  (mult . (id <&> (fact . pre)) ))
      . ((== Zero)?)

mult :: (Nat, Nat) -> Nat
mult (n , Zero ) = Zero
mult (n, Succ a) = add (n, mult (n,a))
--   error "not implemented"
