module ProbabilisticFunctional where


-- https://github.com/VictorCMiraldo/mmm
-- eecs.oregonstate.edu/~erwig/pfp/

import Data.List (sort,sortBy,transpose, delete)
import Control.Monad (Monad(..), MonadPlus(..))

type Probability = Float
  --deriving Show

newtype Dist a = D {unD :: [(a,Probability)]}
  deriving Show

type Spread a = [a] -> Dist a

uniform :: [a] -> Dist a 
uniform l = D [(el,quot) | el <- l]
  where
    quot = 1 / fromIntegral (length l)

die :: Dist Int
die = uniform [1..6]

type Event a = a -> Bool
(??) :: Event a -> Dist a -> Probability
(??) p = sum . map snd . filter (p . fst) . unD

joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f (D d) (D d') = D [(f x y,p*q) | (x,p) <- d, (y,q) <- d']
prod :: Dist a -> Dist b -> Dist (a,b)
prod = joinWith (,)

dice :: Int -> Dist [Int]
dice 0 = D [([], 1.0 )]
dice n = joinWith (:) die (dice (n-1))

instance Monad Dist where
  return x = D [(x, 1)]
  d >>= f  = D [(y,q  * p) | (x,p) <- unD d, (y,q) <- unD (f x)]

instance Applicative Dist where
  pure = return
  f <*> d = do { f'<-f; d' <- d; return $ f' d' }

instance Functor Dist where
  fmap f (D d) = D [(f x,p) | (x,p) <- d]

mapD :: (a -> b) -> Dist a -> Dist b
mapD = fmap

certainly :: a -> Dist a
certainly = return 
fail :: Dist a
fail = D []


(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >@> g = (>>= g) . f

sequ :: Monad m => [a -> m a] -> a -> m a
sequ = foldl (>@>) return


selectOne :: Eq a => [a] -> Dist (a,[a])
selectOne c = uniform [(v,delete v c) | v <- c]

selectMany :: Eq a => Int -> [a] -> Dist ([a],[a])
selectMany 0 c = return ([],c)
selectMany n c = do 
                  (x,c1) <- selectOne c
                  (xs,c2) <- selectMany (n-1) c1
                  return (x:xs,c2)

select :: Eq a => Int -> [a] -> Dist [a]
select n = mapD (reverse . fst) . selectMany n

-- 2 sixes from throwing 4 dices
q1 = ((>=2) . length . filter (==6)) ?? dice 4

-- “What is the probability of drawing a red, green, and blue marble 
-- (in this order) from a jar containing two red, two green, 
-- and one blue marble without putting them back?”
data Marble = R | G | B
  deriving Eq
q2 = (==[R,G,B]) ?? select 3 [R,R,G,G,B]


-- 2. The Monty Hall Problem

-- Transition
type Trans a = a -> Dist a

