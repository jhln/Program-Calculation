{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module AnnimationHudak where


type Time = Double
type Annimation a = Time -> a

newtype Behavior a = Beh (Time -> a)

instance Eq (Behavior a) where
  al==a2 = error "Can't compare behaviors"

instance Show (Behavior a) where
  showsPrec n al = error "« Behavior » "

instance Num a => Num (Behavior a) where
  (+) = lift2 (+)
  (*) = lift2 (*)
  negate = lift1 negate
  abs = lift1 abs
  signum = lift1 signum
  fromInteger = lift0 . fromInteger

instance Fractional a => Fractional (Behavior a) where
  (/) = lift2 (/)
  fromRational = lift0 . fromRational

instance Floating a => Floating (Behavior a) where
  pi = lift0 pi
  sqrt = lift1 sqrt
  exp = lift1 exp
  log = lift1 log
  sin = lift1 sin
  cos = lift1 cos
  tan = lift1 tan
  asin = lift1 asin
  acos = lift1 acos
  atan = lift1 atan
  sinh = lift1 sinh
  cosh = lift1 cosh
  tanh = lift1 tanh
  asinh = lift1 asinh
  acosh = lift1 acosh
  atanh = lift1 atanh


lift0 :: a -> Behavior a
lift0 x = Beh (\f -> x)
lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f (Beh a) = Beh (\t -> f (a t))
lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 g (Beh a) (Beh b) = Beh (\t -> g (a t) (b t))
lift3 :: (a -> b -> c -> d) -> (Behavior a -> Behavior b -> Behavior c -> Behavior d)
lift3 g (Beh a) (Beh b) (Beh c) = Beh (\t -> g (a t) (b t) (c t))


--time :: Behavior Time
--time = Beh (\t -> t)

data UserAction 
  = Key {char :: Char, isDown :: Bool}
  | Button {pt:: Point, isLeft, isDown :: Bool}
  | MouseMove {pt:: Point}
  | Resize
  | Closed
  deriving (Show)

type Point = (Integer, Integer)


newtype Behavior1 a 
  = Behavior1 ([(UserAction, Time)] -> Time -> a)
newtype Behavior2 a 
  = Behavior2 ([(UserAction, Time)] -> [Time] -> [a])

newtype Behavior' a = 
    Behavior' (([Maybe UserAction], [Time]) -> [a])
    --deriving (Functor, Applicative, Monad)
newtype Event' a = 
  Event' (([Maybe UserAction], [Time]) -> [Maybe a])


time :: Behavior' Time
time = Behavior' $ \(_, ts) -> ts


constB :: a -> Behavior' a
constB x = Behavior' (\_ -> repeat x)