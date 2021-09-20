module FinancialContracts where

import Control.Applicative
import Prelude hiding (truncate)

---- do it as parsing combinators from haskell lib
ownPartition :: String -> String -> [String] -> String -> [String]
ownPartition _ [] container [] = container
ownPartition _ actual container [] = actual:container
ownPartition [c] actual container (h:r)
  | c /= h      = ownPartition [c] (actual ++ [h]) container r
  | null actual = ownPartition [c] [] container r
  | otherwise   = ownPartition [c] [] (actual:container) r


c1 :: Contract
c1 = zcb t1 100 GBP -- Nothing Nothing


newtype Date = D Double 
  deriving (Show, Eq, Ord)

instance Num Date where
  (D x) + (D y) = D $ x + y
  (D x) * (D y) = D $ x * y
  abs (D x) = D $ abs x
  negate (D x) = D $ negate x
  signum (D x) = D $ signum x
  fromInteger = D . fromIntegral -- fromInteger



date :: String -> Date
date = D . read . concat . ownPartition "." [] []

t1,t2 :: Date
t1 = date "01.01.2010"
t2 = date "01.02.2010"

data Currency = EUR | USD | GBP


type Creditor = Maybe Person
type Debtor = Maybe Person

newtype Person = P Int

type Days = Double -- A time difference
-- diff :: Date -> Date -> Days
-- add :: Date -> Days -> Date

(&&&) :: Contract -> Contract -> Contract
(&&&) = And

c2,c3 :: Contract
c2 = zcb t2 200 GBP
c3 = c1 &&& c2

give :: Contract -> Contract
give = Give


andGive :: Contract -> Contract -> Contract
andGive c d = c &&& (give d)

c4' = c1 `andGive` c2

c5 = one GBP

one :: Currency -> Contract
one = One

c6 = get (truncate t1 (one GBP))
truncate :: Date -> Contract -> Contract
truncate = Truncate
get :: Contract -> Contract
get = Get

c7 = scaleK 100 (get (truncate t1 (one GBP)))
scaleK :: Double -> Contract -> Contract
-- scaleK = Scale . Obs
scaleK = scale . konst

zero :: Contract
zero = Zero

(|||) :: Contract -> Contract -> Contract
(|||) = Or

anytime :: Contract -> Contract
anytime = Anytime

(|->) :: Contract -> Contract -> Contract
(|->) = Then

data Contract = C Date Double Currency -- Creditor Debtor
              | And Contract Contract -- Creditor Debtor
              | Or Contract Contract
              | Give Contract -- changed obligations
              | Zero 
              | One Currency -- immidiatly 1, no horizont
              | Truncate Date Contract -- trims the temporal horizont
              | Then Contract Contract
              | Get Contract -- aquires the contract at last temporal horizont
              | Scale (Obs Double) Contract
              | Anytime Contract
  
zcb' :: Date -> Double -> Currency -> Contract
zcb' = C

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = scaleK x (get (truncate t (one k)))


{-
Combinator Description

zero :: Contract
zero is a contract that may be acquired at any
time. It has no rights and no obligations, and
has an infnite horizon. (Section 3.4.) 

one :: Currency -> Contract
(one k) is a contract that immediately pays the
holder one unit of the currency k. The contract
has an infnite horizon. (Section 3.2.)

give :: Contract -> Contract
To acquire (give c) is to acquire all c's rights as obligations, and vice versa. Note that for a bilateral contract q between parties A and B, A
acquiring q implies that B acquires (give q). (Section 2.2.)

and :: Contract -> Contract -> Contract
If you acquire (c1 `and` c2) then you immediately acquire both c1 (unless it has expired) and
c2 (unless it has expired). The composite contract expires when both c1 and c2 expire. (Section 2.2.) 

or :: Contract -> Contract -> Contract
If you acquire (c1 `or` c2) you must immediately acquire either c1 or c2 (but not both). If
either has expired, that one cannot be chosen.
When both have expired, the compound contract
expires. (Section 3.4.)

truncate :: Date -> Contract -> Contract
(truncate t c) is exactly like c except that it
expires at the earlier of t and the horizon of c. Notice that truncate limits only the possible ac- quisition date of c; it does not truncate c's rights
and obligations, which may extend well beyond
t. (Section 3.4.)

then :: Contract -> Contract -> Contract
If you acquire (c1 `then` c2) and c1 has not
expired, then you acquire c1. If c1 has expired,
but c2 has not, you acquire c2. The compound
contract expires when both c1 and c2 expire.
(Section 3.5.)

scale :: Obs Double -> Contract -> Contract
If you acquire (scale o c), then you acquire c
at the same moment, except that all the rights
and obligations of c are multiplied by the value
of the observable o at the moment of acquisition.
(Section 3.3.)

get :: Contract -> Contract
If you acquire (get c) then you must acquire c
at c's expiry date. The compound contract expires at the same moment that c expires. (Section 3.2.)

anytime :: Contract -> Contract
If you acquire (anytime c) you must acquire c, but you can do so at any time between the acquisition of (anytime c) and the expiry of c. The
compound contract expires when c does. (Section 3.5.)

-}

--------- Observeble

data Obs a = Obs a
              | Konst a
              | Time a
              | Lift (a -> a) (Obs a)
              | Lift2 (a -> a -> a) (Obs a) (Obs a)


instance Num a => Num (Obs a) where
  fromInteger = konst . fromInteger
  (+)         = lift2 (+)
  (-)         = lift2 (-)
  (*)         = lift2 (*)
  abs         = lift abs
  signum      = lift signum

lift' :: (a -> b) -> Obs a -> Obs b
lift' f (Obs x) = Obs $ f x
lift2' :: (a->b->c) -> Obs a -> Obs b -> Obs c
lift2' f (Obs x) (Obs y) = Obs $ f x y

lift :: (a -> a) -> (Obs a) -> (Obs a)
lift = Lift
lift2 :: (a -> a -> a) -> (Obs a) -> (Obs a) -> (Obs a)
lift2 = Lift2

noonTempInLA :: Obs Double
noonTempInLA = Obs 1.6
libor3m :: Obs Double
libor3m = Obs 3.4

c8 = scale noonTempInLA (one USD)

scale :: Obs Double -> Contract -> Contract
scale = Scale
konst :: a -> Obs a
konst = Konst 

ntLAinKelvin :: Obs Double
ntLAinKelvin = noonTempInLA + konst 373


european :: Date -> Contract -> Contract
european t u = get (truncate t (u ||| zero))

c5' = european (date "24 Apr 2003") (
  zcb (date "12.05.2003") 0.4 GBP &&&
  zcb (date "12.05.2004") 9.3 GBP &&&
  zcb (date "12.05.2005") 109.3 GBP &&&
  give (zcb (date "26.4.2003") 100 GBP))

perhaps :: Date -> Contract -> Contract
perhaps t u = truncate t (u ||| zero)

american (t1,t2) u = (get (truncate t1 opt)) |-> opt
  where
    opt :: Contract
    opt = anytime (perhaps t2 u)



--------------- Semantics

data Horizont = H Date | Inf
  deriving (Show, Eq, Ord)

h :: Contract -> Horizont
h (C d _ _) = H d
h (And c1 c2) = max (h c1) (h c2)
h (Or c1 c2) = max (h c1) (h c2)
h (Give c) = h c
h Zero = Inf
h (One _) = Inf 
h (Truncate d c) = min (H d) $ h c
h (Then c1 c2) = max (h c1) (h c2)
h (Get c) = h c
h (Scale _ c) = h c
h (Anytime c) = h c

instance Num a => Num (Maybe a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger


eval :: Horizont -> Contract -> Maybe Double
eval t (C d v c) 
  | t == (H d) = Just v
  | otherwise = Nothing
eval t (And c1 c2)
  | t <= h1 && t <= h2 = (eval t c1) + (eval t c2)
  | t <= h1 = (eval t c1)
  | t <= h2 = (eval t c2)
  | otherwise = Nothing
  where
    (h1, h2) = (h c1, h c2)
eval t (Or c1 c2)
  | t <= h1 && t <= h2 = min (eval t c1) (eval t c2)
  | t <= h1 = (eval t c1)
  | t <= h2 = (eval t c2)
  | otherwise = Nothing
  where
    (h1, h2) = (h c1, h c2)
eval t (Give c) = - (eval t c)
eval _ Zero = Just 0
eval _ (One c) = Just 1
eval t (Truncate d c)
  | (H d) <= t = eval t c
  | otherwise = Nothing
eval t (Then c1 c2)
  | t <= (h c1) = eval t c1
  | otherwise = eval t c2
eval t (Get c)
  | h c == Inf = eval t c
  | otherwise = fmap (1 / 2 *) $ eval t c
eval t (Anytime c)
  | h c == Inf = eval t c
  | otherwise = fmap (1 / 2 *) $ eval t c
eval t (Scale o c) = Just (evalO t o) * (eval t c)

evalO :: Horizont -> Obs Double -> Double
evalO t (Obs a) = a
evalO t (Konst a) = a
evalO (H (D t)) (Time s) = t - s
evalO t (Lift f o) = f $ evalO t o
evalO t (Lift2 f o1 o2) = f (evalO t o1) (evalO t o2)

