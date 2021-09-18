module ProbabilisticFunctional where


-- https://github.com/VictorCMiraldo/mmm
-- eecs.oregonstate.edu/~erwig/pfp/

import Data.List (sort,sortBy,transpose, delete, (\\))
import Control.Monad (Monad(..), MonadPlus(..))
import System.Random

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
  deriving (Eq, Enum)

doors :: [Door]
doors = [A .. C]

data State = Doors {prize :: Door,  -- contains the prize
                    chosen :: Door, -- is chosen
                    opened :: Door} -- is opened

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
  deriving Show

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
pick d = Random.randomRIO (0,1) >>= return . selectP d

random :: Trans a -> RChange a
random t = pick . t


type RDist a = R (Dist a)

type RTrans a = a -> RDist a

rDist :: Ord a => [R a] -> RDist a
rDist = fmap (norm . uniform') . sequence





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
