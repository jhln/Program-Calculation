module ComposingContractCombinator where



data Contract = One Date Currency
              | Give Contract
              | And Contract Contract
              | ZCB Date Double Currency

type Date = Int
data Currency = EUR | USD | GBP

t1,t2 :: Date
t1 = 30213
t2 = 40213

c1 :: Contract
c1 = zcb t1 100 GBP


----- Constructor Combinator


zcb :: Date -> Double -> Currency -> Contract
zcb = ZCB 

c2,c3 :: Contract
c2 = zcb t2 200 GBP


----- And Combinator

andC :: Contract -> Contract -> Contract
andC = And 
c3 = c1 `andC` c2

{-
-- giveC :: Contract -> Contract

c4 = c1 `and` give c2

andGive :: Contract -> Contract -> Contract
andGive c d = c `and` give d

c4' = c1 `andGive` c2
-}


----- Give Combinator

giveC :: Contract -> Contract
giveC = undefined



----- Oberservables

data Obs a = Obs a
  deriving Show

instance Num a => Num (Obs a) where
  fromInteger = konst . fromInteger
  (+)         = lift2 (+)
  (-)         = lift2 (-)
  (*)         = lift2 (*)
  abs (Obs x) = Obs $ abs x
  signum (Obs x) = Obs $ signum x

now :: Date
now = 0

date :: Obs Date
date = Obs now


----- Unit Combinator

oneC :: Currency -> Contract
oneC = zcb now 1 

c5 = oneC GBP
c6 = scaleC (konst 100) c5

konst :: a -> Obs a
konst = Obs


----- Zero Combinator

zeroC :: Contract
zeroC = undefined



----- temporal Combinator

c7 = whenC (at t1) c6



----- Temporal Operator

at :: Date -> Obs Bool
at t = lift2 (==) date (konst t)

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 f (Obs a) (Obs b) = Obs $ f a b


----- Contract Constructor

zcbC :: Date -> Double -> Currency -> Contract
zcbC t x k = whenC (at t) (scaleC (konst x) (oneC k))


----- Scaling Combinator

scaleC :: Obs Double -> Contract -> Contract
scaleC = undefined

whenC = undefined

rainInCyrus :: Obs Double
rainInCyrus = undefined

c9 = scaleC ((rainInCyrus - 7) * 1000) (oneC USD)

(%<), (%<=), (%==), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<) = lift2 (<)
(%<=) = lift2 (<=)
(%==) = lift2 (==)
(%>) = lift2 (>)
(%>=) = lift2 (>=)

----- Conditional Combinator

condC :: Obs Bool -> Contract -> Contract -> Contract
condC = undefined

c10 = condC (rainInCyrus %> 10) (oneC GBP) (oneC USD)


----- Conditional Combinator

orC :: Contract -> Contract -> Contract
orC = undefined

c10' = zcbC t1 100 GBP `orC` zcbC t2 110 GBP


c11 = europeanC 30424 $
        (zcbC 30512 0.4 GBP) `andC`
        (zcbC 30512 0.4 GBP) `andC`
        (zcbC 30512 0.4 GBP) `andC`
        (giveC $ zcbC 30426 100 GBP)

europeanC :: Date -> Contract -> Contract
europeanC t u = whenC (at t) $ u `orC` zeroC


----- anytime Combinator

anytimeC :: Obs Bool -> Contract -> Contract
anytimeC = undefined

americanC (t1,t2) u = anytimeC (between t1 t2) u


----- window Operator

between :: Obs Date -> Obs Date -> Obs Bool
between t1 t2 = lift2 (&&) (date %>= t1) (date %<= t2)


----- until Combinator

untilC :: Obs Bool -> Contract -> Contract
untilC = undefined

t1',t2' :: Obs Date
t1' = undefined
t2' = undefined

c12 = untilC (interestRate %> konst 6) $ americanC (t1',t2') c1

interestRate :: Obs Int
interestRate = Obs 6


