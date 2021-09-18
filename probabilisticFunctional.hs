module ProbabilisticFunctional where


-- https://github.com/VictorCMiraldo/mmm
-- eecs.oregonstate.edu/~erwig/pfp/

import Data.List (sort,sortBy,transpose, delete, (\\))
import Control.Monad (Monad(..), MonadPlus(..))
import System.Random (randomRIO)



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
(??) p = sumP . filter (p . fst) . unD

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
impossible :: Dist a
impossible = D []


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
data MarbleColor = R' | G' | B'
  deriving Eq
q2 = (==[R',G',B']) ?? select 3 [R',R',G',G',B']



--- 2. The Monty Hall Problem

-- Transition
type Trans a = a -> Dist a

data Outcome = Win | Lose
  deriving (Show, Eq)

firstChoice :: Dist Outcome
firstChoice = uniform [Win,Lose,Lose]

switch' :: Trans Outcome
switch' Win = certainly Lose
switch' Lose = certainly Win

switchedChoice :: Dist Outcome
switchedChoice = firstChoice >>= switch'

data Door = A | B | C
  deriving (Eq, Enum, Ord)

doors :: [Door]
doors = [A .. C]

data State = Doors {prize :: Door,  -- contains the prize
                    chosen :: Door, -- is chosen
                    opened :: Door} -- is opened
                    deriving (Eq, Ord)

-- inital state
start :: State
start = Doors 
          {prize=u,chosen=u,opened=u} 
          where 
            u=undefined

hide :: Trans State
hide s = uniform [s{prize=d} | d <- doors]

choose :: Trans State
choose s = uniform [s{chosen=d} | d <- doors]

open :: Trans State
open s = uniform [ s{opened=d} 
                 | d <- doors \\ [prize s,chosen s]]


--- Strategies

type Strategy = Trans State

switch :: Strategy
switch s = uniform [ s{chosen=d} 
                   | d <- doors \\ [chosen s,opened s]]

stay :: Strategy
stay = certainlyT id

--- function converter
certainlyT :: (a -> a) -> Trans a
certainlyT f = certainly . f


--- game round
game :: Strategy -> Trans State
game s = sequ [hide,choose,open,s]

--- winning condition
result :: State -> Outcome
result s = if chosen s==prize s then Win else Lose


--- computes the distribution for a strategy
eval :: Strategy -> Dist Outcome
eval s = mapD result (game s start)



--- 3. Biology: Tree Growth

type Height = Int

--- states of the tree
data Tree = Alive Height | Hit Height | Fallen
  deriving (Show, Ord, Eq)

--- grows if alive between 1 and 5 every year
grow :: Trans Tree
grow (Alive h) = normal [Alive k | k <- [h+1..h+5]]

normal :: Spread a
normal = shape (normalCurve 0.5 0.5)

hit :: Trans Tree
hit (Alive h) = certainly (Hit h)

fall :: Trans Tree
fall _ = certainly Fallen

evolve :: Trans Tree
evolve t@(Alive _) = unfoldT (enum [0.9,0.04,0.06] [grow,hit,fall]) t
evolve t = certainly t

enum :: [Probability] -> Spread a
enum ps xs = mkD $ zip xs ps

checkD :: Dist a -> Dist a
checkD (D d) | abs (1-sumP d) < errorMargin = D d
             | otherwise = error ("Illegal distribution: total probability = "++show (sumP d))

mkD :: [(a,Probability)] -> Dist a
mkD = checkD . D

errorMargin :: Probability
errorMargin = 0.00001

unfoldT :: Dist (Trans a) -> Trans a
unfoldT (D d) x = D [ (y,p*q) | (f,p) <- d, (y,q) <- unD (f x) ]

treeGrowth :: Int -> Tree -> Dist Tree
treeGrowth n = n *. evolve

seed :: Tree
seed = Alive 0

class Iterate c where
  (*.) :: Int -> (a -> c a) -> (a -> c a)
  while :: (a -> Bool) -> (a -> c a) -> (a -> c a)
  until :: (a -> Bool) -> (a -> c a) -> (a -> c a)
  until p = while (not . p)

instance Iterate Dist where
  0 *. t = t
  i *. t = (>>= t) . ((i-1) *. t)
  while = undefined

treeGrowthYears :: Dist Tree
treeGrowthYears = treeGrowth 3 seed 


--- 4. Randomized change to reduce exponential Growth

type R a = IO a

type RChange a = a -> R a

pick :: Dist a -> R a
pick d = randomRIO (0,1) >>= return . selectP d

random :: Trans a -> RChange a
random t = pick . t


type RDist a = R (Dist a)

type RTrans a = a -> RDist a

rDist :: Ord a => [R a] -> RDist a
rDist = fmap (norm . uniform') . sequence


class Sim c where
  (~.) :: Ord a => Int -> (a -> c a) -> RTrans a
  (~*.) :: Ord a => (Int,Int) -> (a -> c a) -> RTrans a
  (~..) :: Ord a => (Int,Int) -> (a -> c a) -> RExpand a

instance Sim IO where
  (~.) n t = rDist . replicate n . t
  (~*.) (k,n) t = k ~. (n *. t)
  (~..) (k,n) t = mergeTraces . replicate k . rWalk n t

instance Sim Dist where
  (~.) n = (~.) n . random
  (~*.) x = (~*.) x . random
  (~..) x = (~..) x . random

instance Iterate IO where
  0 *. t = t
  i *. t = (>>= t) . ((i-1) *. t)
  while = undefined


--- MontiHall randomized
simEval :: Int -> Strategy -> RDist Outcome
simEval k s = mapD result `fmap` (k ~. game s) start

--- treeGrowth randomized
simTreeGrowth :: Int -> Int -> Tree -> RDist Tree
simTreeGrowth k n = k ~. treeGrowth n


simTreeGrowth' :: Int -> Int -> Tree -> RDist Tree
simTreeGrowth' k n = (k,n) ~*. evolve


--- 5. Tracing and randomized tracing

type Trace a = [a]
type Space a = Trace (Dist a)
type Walk a = a -> Trace a
type Expand a = a -> Space a

type Change a = a -> a

walk :: Int -> Change a -> Walk a
walk n f = take n . iterate f


(*..) :: Int -> Trans a -> Expand a
0 *.. _ = singleton . certainly
1 *.. t = singleton . t
n *.. t = t >>: ((n-1) *.. t)

singleton :: a -> [a]
singleton x = [x]


(>>:) :: Trans a -> Expand a -> Expand a
f >>: g = \x -> 
            let ds@(D d:_)=g x in
              D [(z,p*q) | (y,p) <- d,(z,q)<- unD (f y)]:ds

type RTrace a = R (Trace a)
type RSpace a = R (Space a)
type RWalk a = a -> RTrace a
type RExpand a = a -> RSpace a



-- rWalk computes a list of values by
-- randomly selecting one value from a distribution in each step.
-- 
rWalk :: Int -> RChange a -> RWalk a
rWalk 0 _ = return . singleton
rWalk 1 t = (>>= return . singleton) . t
rWalk n t = composelR t (rWalk (n-1) t)

--          (a -> m a) -> (a -> m [a]) -> (a -> m [a])
composelR :: RChange a -> RWalk a -> RWalk a
composelR f g x = do {rs@(r:_) <- g x; s <- f r; return (s:rs)}




-- mergeTraces converts a list of RTraces, into a list of randomized 
--             distributions, i.e., an RSpace, by creating a randomized
--             distribution for each list position across all traces
--
mergeTraces :: Ord a => [RTrace a] -> RSpace a
mergeTraces = fmap (zipListWith (norm . uniform)) . sequence
              where
                zipListWith :: ([a] -> b) -> [[a]] -> [b]
                zipListWith f = map f . transpose


--- simultaing TreeGrowth History
treeGrowthHist :: Int -> Tree -> Space Tree
treeGrowthHist n = n *.. evolve

--- simultaing randomized TreeGrowth History
simHist :: Int -> Int -> Tree -> RSpace Tree
simHist k n = (k,n) ~.. evolve


-- selecting from distributions
-- 
selectP :: Dist a -> Probability -> a
selectP (D d) p = scanP p d

scanP :: Probability -> [(a,Probability)] -> a
scanP p ((x,q):ps) | p<=q || null ps = x
                   | otherwise       = scanP (p-q) ps


-- normalization = grouping
-- 
normBy ::  Ord a => (a -> a -> Bool) ->  Dist a -> Dist a
normBy f = onD $ accumBy f . sort

onD :: ([(a,Probability)] -> [(a,Probability)]) -> Dist a -> Dist a
onD f  = D . f . unD

accumBy :: Num b => (a -> a -> Bool) -> [(a,b)] -> [(a,b)]
accumBy f ((x,p):ys@((y,q):xs)) | f x y     = accumBy f ((x,p+q):xs)
                                | otherwise = (x,p):accumBy f ys
accumBy _ xs = xs

norm ::  Ord a => Dist a -> Dist a
norm = normBy (==)

norm' :: Ord a => [(a,Probability)] -> [(a,Probability)]
norm' = accumBy (==) . sort



-----

uniform' :: Spread a
uniform' = shape (const 1)

shape :: (Float -> Float) -> Spread a
shape _ [] = impossible
shape f xs = scale (zip xs ps)
             where incr = 1 / fromIntegral ((length xs) - 1)
                   ps = map f (iterate (+incr) 0)

scale :: [(a,Probability)] -> Dist a
scale xs = D (map (\(x,p)->(x,p/q)) xs)
           where q = sumP xs

sumP :: [(a,Probability)] -> Probability
sumP = sum . map snd


normalCurve :: Float -> Float -> Float -> Float
normalCurve mean stddev x 
  = 1 / sqrt (2 * pi) * exp (-1/2 * u^2)
  where 
    u = (x - mean) / stddev
