{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module TracingMonad where

import Prelude hiding (drop, round)
import Data.List (nub)
import Control.Monad.Writer (MonadWriter(..), Writer(..), liftM)


-- https://arxiv.org/pdf/1202.2922.pdf


--- 2. Tracing with MonadTrans Free

data Free f a = Wrap (f (Free f a))
              | Return a

instance Functor f => Functor (Free f) where
  fmap g (Return a) = Return $ g a
  fmap g (Wrap f) = Wrap $ fmap (fmap g) f

instance Functor f => Monad (Free f) where
  return = Return
  (Return f) >>= f' = f' f
  (Wrap f) >>= f' = Wrap $ fmap (>>= f') f
  -- xs >>= f = join (fmap f xs) where
    -- join (Return f) = f
    -- join (Wrap f) = Wrap (fmap join f)

instance Functor f => Applicative (Free f) where
  pure = Return
  m1 <*> m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }
  --(<*>) = ap
  {-
  (Return a) <*> (Return b) = Return $ a b
  (Return a) <*> (Wrap b) = Wrap $ fmap a b
  (Wrap a) <*> b = Wrap $ (<*> b) <$> a
  -}


class (Monad m, Monad t) => MonadTrans m t | t -> m where
  lift ::m a -> t a
  drop ::t a -> m a

instance (Functor m, Monad m) => MonadTrans m (Free m) where
  lift = Wrap . fmap Return
  drop (Return a) = return a
  drop (Wrap m) = m >>= drop

-- examples missing


--- 3. MonadTrace

class Monad t => MonadTrace t where
  mark ::t ()

mind :: (MonadTrans m v, MonadTrace v) => m a -> v a
mind m = do {x <- lift m; mark; return x}


-- c = drop $ do {x <- lift (Just 1); y <- mind (Just 2); z <- mind ((Just 3)); return (x+y+z)}


newtype Nest m a = Nest { unNest :: m (Free m a) }

instance Functor m => Functor (Nest m) where
  fmap f (Nest m) = Nest $ fmap (fmap f) m

instance (Functor m, Monad m) => Monad (Nest m) where
  return = Nest . return . return
--  xs >>= f = join (fmap f xs) where
  Nest m >>= g = Nest $ m >>= iter
    where
      -- g :: a -> Nest m b
      -- iter :: Free m a -> m (Free m b)
      iter (Return a) = unNest $ g a
      iter (Wrap m) = return $ Wrap $ m >>= iter

instance Monad m => Applicative (Nest m) where
  pure = return
  m1 <*> m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }
  --(<*>) = ap

instance (Functor m, Monad m) => MonadTrans m (Nest m) where
  lift = Nest . fmap Return
  drop v = unNest v >>= revert
    where
      revert (Return a) = return a
      revert (Wrap m) = m >>= revert

instance (Functor m, Monad m) => MonadTrace (Nest m) where
  mark = (Nest . return . Wrap . return . Return) ()

-- 0. spät nachts, heimweg, auch lang, wichtig
-- 1. vermischen,
-- 2. intensity,
-- 3. home working at home,
-- 4. telefonieren in der wohnung
-- 5. nicht mitbekommen bei einmal besuchen


--- 4. Interpreting traces
--- 4.1 The partiallity monad

data Partial a = Later (Partial a) | Now a

collatz::Integer -> Bool
collatz 1 = True
collatz n
  | odd n     = collatz (3*n+1)
  | otherwise = collatz (n `div` 2)



newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
--  fmap f a = Identity $ f $ runIdentity a
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity a) <*> (Identity b) = Identity $ a b

instance Monad Identity where
  return = Identity
  (Identity a) >>= b = b a


collatzN ::Integer -> Nest Identity Bool
collatzN 1 = return True
collatzN n
  | odd n     = mark >> collatzN (3*n+1)
  | otherwise = mark >> collatzN (n `div` 2)


(?) :: Nest Identity Bool -> Nest Identity Bool -> Bool
(Nest (Identity (Return False))) ? (Nest (Identity (Return False))) = False
Nest (Identity (Return True)) ? _ = True
_ ? Nest (Identity (Return True)) = True
Nest (Identity (Wrap m)) ? x = x ? Nest m


--- 4.2 Approximating computations

newtype Distr a = Distr { runDistr :: [(Double,a)]}
  deriving Show

instance Functor Distr where
  fmap f (Distr xs) = Distr (fmap (\ (p,a) -> (p, f a)) xs)

instance Applicative Distr where
  pure = return
  m1 <*> m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }

instance Monad Distr where
  return a = Distr [(1,a)]
  a >>= b = join $ fmap b a where
    join (Distr xs) = Distr [(p*q,a) | (p,Distr ys) <- xs, (q,a) <- ys]

coin ::Distr Int
coin = Distr [(0.5,0),(0.5,1)]

-- endless loop !
third ::Distr Int
third =
  do
    x <- coin
    y <- coin
    case (x, y) of
      (1,1) -> return 0
      (1,0) -> return 1
      (0,1) -> return 2
      (0,0) -> third

thirdN ::Nest Distr Int
thirdN = do
  x <- lift coin
  y <- lift coin
  case (x, y) of
    (1,1) -> return 0
    (1,0) -> return 1
    (0,1) -> return 2
    (0,0) -> mark >> thirdN

takeN :: (Functor m, Monad m) => Int -> Nest m a -> Nest m (Maybe a)
takeN k (Nest m) = Nest (fmap (aux k) m)
  where
    aux 0 (Wrap _) = Return Nothing
    aux k (Wrap m) = Wrap (fmap (aux (k - 1)) m)
    aux k (Return a) = Return (Just a)

approx :: (Functor m, Monad m) => Int -> Nest m a -> m (Maybe a)
approx k = drop . takeN k

simpl :: Eq a => Distr a -> Distr a
simpl (Distr xs) =
  Distr (map
          (\ x -> (sum [p | (p,a) <- xs, x == a], x))
          (nub (fmap snd xs)))


--- 4.3 Prolog Cut operator

call :: Nest [] a -> [a]
call (Nest xs) = aux xs
  where
    aux [] = []
    aux (Return a : xs) = a : aux xs
    aux (Wrap as : _ ) = aux as

brace :: Nest [] a -> Nest [] a
brace = lift . call

cut :: Nest [] ()
cut = mark

cutted = call (
  do
    x <- lift [4,7,13,9]
    y <- lift [2,8,1]
    when (x+y > 15) cut
    return (x+y))

when b m = if b then m else return ()

cutted_nested =
  call (
    do
      x <- lift [4,7,13,9]
      brace ( do
                y <- lift [2,8,1]
                when (x+y > 15) cut
                return (x+y) ) )


--- 4.4 Poor man's concurrency transformer, revisited

data Action m a = Par (Action m a) (Action m a)
                | Act (m (Action m a))
                | Done a
                | Kill

instance Functor m => Functor (Action m) where
  fmap f (Par a b)  = Par (fmap f a) (fmap f b)
  fmap f (Act m)    = Act $ fmap (fmap f) m
  fmap f (Done a)   = Done $ f a
  fmap _ Kill       = Kill

instance Functor m => Applicative (Action m) where
  pure = Done
  m1 <*> m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }

instance Functor m => Monad (Action m) where
  return = Done
  (Par a b) >>= f = Par (a >>= f) (b >>= f)
  (Act m) >>= f = Act (fmap (>>= f) m)
  (Done a) >>= f = f a
  Kill >>= f = Kill


type Concurrent m = Nest (Action m)

done :: (Monad m) => a -> Concurrent m a
done = lift . Done
kill ::(Monad m) => Concurrent m a
kill = lift Kill
act :: (Monad m) => m a -> Concurrent m a
act m = lift (Act (liftM Done m))
par :: (Monad m) => Concurrent m a -> Concurrent m a -> Concurrent m a
par (Nest m1) (Nest m2) = Nest (Par (Done (Wrap m1)) (Done (Wrap m2)))
fork :: (Monad m) => Concurrent m b -> Concurrent m ()

fork m = par (m >> kill) (act (return ()))

round :: Monad m => [Nest (Action m) x] -> m [x]
round [] = return []
round (Nest w : as) = case w of
  Kill            -> round as
  Done (Return x) -> do {xs <- round as; return (x : xs)}
  Done (Wrap a)   -> round (as++[Nest a])
  Act m           -> do {a <- m; round ([Nest a] ++ as)}
  Par a b         -> round ([Nest b] ++ as ++[Nest a])


instance (Monoid s) => MonadWriter s (Concurrent (Writer s)) where
  tell = act . tell
  listen = undefined
  pass = undefined

cat ::Concurrent (Writer String) Int
cat = replicateM 5 (tell "cat" >> mark) >> return 1

fish ::Concurrent (Writer String) Int
fish = replicateM 7 (tell "fish" >> mark) >> return 2

fish' ::Concurrent (Writer String) Int
fish' = replicateM 7 (tell "fish") >> return 2



paraRun1 = round [ do
                    x <- fish `par` cat
                    tell "dog"
                    return x]

paraRun2 = round [ do
                    fork fish
                    x <- cat
                    tell "dog"
                    return x]

paraRun3 = round [ do
                    fork fish0
                    x <- cat
                    tell "dog"
                    return x]


--- 5  Poor man's concurrency transformer, revisited


data Trace s a = TCons s (Trace s a) | Nil a
newtype States s a = States {runStates :: s -> Trace s a}

-- Monad (States s)
-- MonadTrans (States s) (States s)
-- MonadTrace (States s)
