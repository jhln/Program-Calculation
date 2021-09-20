module FunctionalReactiveAnimation where

type Time = Integer

data Ivl a = a `Upto` a

data Behavior a = Behavior 
                    (Time -> (a, Behavior a))
                    (Ivl Time -> (Ivl a, Behavior a))


-- unchanged behaviour
time :: Behavior Time
time = Behavior 
        (\ t -> (t, time))
        (\ iv -> (iv, time))

lift2 f fi b1 b2 = Behavior sample isample
  where 
    sample t = (f x1 x2, lift2 f fi b1' b2')
      where 
        (x1, b1') = b1 `at` t
        (x2, b2') = b2 `at` t
    isample iv = (fi xi1 xi2, lift2 f fi b1' b2')
      where 
        (xi1, b1') = b1 `during` iv
        (xi2, b2') = b2 `during` iv


at = undefined
during = undefined

----- euqality testing function for intervals
(lo1 `Upto` hi1) ==# (lo2 `Upto` hi2)
  | hi1 < lo2 || hi2 < lo1 = False `Upto` False
  | lo1==hi1 && lo2==hi2 && lo1==lo2 = True `Upto` True
  | otherwise = False `Upto` True