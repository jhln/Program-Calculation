{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

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



-----------------------------------------------------
--- Additional Combinatorics


genAcc :: [] a -> [] Int -> [[[[a]]]]
genAcc = undefined

-- gen :: [] a -> Int -> [] ([] a)
gen list 0 = [ [] ]
gen list n = [ new:old | new <- list, old <- gen list (n-1) ]


--genGen (i:restI) list = [genGen restI $ gen l i | l <- list]

nOverK :: (Eq a, Num t, Eq t) => [a] -> t -> [[a]]
nOverK list 0 = [[]]
nOverK list n = [ now : rest | now <- list, rest <- nOverK (eliminated now list) (n-1)]
   where
      eliminated :: Eq a => a -> [a] -> [a]
      eliminated e (e':r)
         | e == e'   = r
         -- | otherwise = e' : eliminated e r
         -- different ideas of order, either by eliminated or by list comprehension
         | otherwise = eliminated e r
      eliminated _ [] = error "not valid"
-- rewrite in do notation

testGen0 :: [[Integer]]
testGen0 = nOverK [1,2,3] 2
testGen1 :: [[[Integer]]]
testGen1 = flip gen 2 $ nOverK [1,2,3] 2
testGen2 :: [[[[Integer]]]]
testGen2 = flip gen 2 $ flip gen 2 $ nOverK [1,2,3] 2

--genGen [] listList = [[]]
--genGen (i:restI) listList = [ gen (now:old) i | now <- listList, old <- genGen restI listList ]

---


{-
powerList [] = [[]]
powerList (x:xs) = plOfXs ++ [x:l| l <- plOfXs]
  where
    plOfXs = powerList xs
-}
-- wrong
powerList' [] = [[]]
powerList' (x:xs) = [] :  [x:l | l <- powerList' xs]

--korrekt, und in der art aufgezählt, wie man es machen würde
powerList'' [] = [[]]
powerList'' list = [] :  [e:l | e <- list
                              , l <- powerList'' 
                                     $ eliminated e list]
  where
      eliminated :: Eq a => a -> [a] -> [a]
      eliminated e (e':r)
         | e == e'   = r
         | otherwise = eliminated e r
      eliminated _ [] = error "not valid"                                     


data S = Void deriving (Eq,Show)
empty' :: [S]
empty' = []

pl2 :: [Integer] -> [[[Integer]]]
pl2 list = powerList'' $ powerList'' list

-- as foldl (b -> a -> b) b (t a) ?
intersectMany :: Eq a => [[a]] -> [a]
intersectMany [] = error "nothing to intersect"
intersectMany [a,b] = intersect a b
intersectMany (a:b:rest) = intersectMany $ (intersect a b) : rest

intersect [] _ = []
intersect (x:xs) s
  | elem x s = x : intersect xs s
  | otherwise = intersect xs s

unionMany :: Eq a => [[a]] -> [a]
unionMany = foldl union []
union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys = x : union xs (delete x ys)


--- Set Data Type


newtype Set a = Set [a] deriving Show

instance Eq a => Eq (Set a) where
  set1 == set2 
    = subSet set1 set2 && subSet set2 set1
{-
instance (Show a) => Show (Set a) where
  showsPrec _ (Set s) str = showSet s str

showSet [] str = showString "{}" str
showSet (x:xs) str = showChar '{' (shows x (showl xs str))
    where 
      showl [] str = showChar '}' str
      showl (x:xs) str = showChar ',' (shows x (showl xs str))
-}
emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _ = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet x (Set s) = elem x s

subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set

insertSet :: (Eq a) => a -> Set a -> Set a
insertSet x (Set ys) 
  | inSet x (Set ys) = Set ys
  | otherwise = Set (x:ys)

deleteSet :: Eq a => a -> Set a -> Set a
deleteSet x (Set xs) = Set (delete x xs)

list2set :: Eq a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)

powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = Set (map (\xs -> (Set xs)) (powerList'' xs))

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs)

(!!!) :: Eq a => Set a -> Int -> a
(Set xs) !!! n = xs !! n


--- Hierarchy

empty,v0 :: Set S
empty = Set []
v0 = empty
v1 = powerSet v0
v2 = powerSet v1
v3 = powerSet v2
v4 = powerSet v3
v5 = powerSet v4
-- ...



-- Problem with infinite type
--powerPower 0 set = Set []
--powerPower n set = powerSet $ powerPower (n-1) set


data Free a = Spread [Free a] | Pure a
  deriving (Functor, Applicative, Monad)

base1 :: Free [Integer]
base1 = Pure [1,2,3]

base2 :: Free Integer
base2 = Spread [Pure 1, Pure 2]

{- 
powerList [] = [[]]
powerList (x:xs) = plOfXs ++ [x:l| l <- plOfXs]
  where
    plOfXs = powerList xs
-}

--powerFree :: Free a -> [Free a]
powerFree (Pure e) = [Spread [Pure e]]
powerFree (Spread []) = [Spread []]
powerFree (Spread (x:xs)) 
  = [Spread new | new <- plOfXs ++ [x:l| l <- plOfXs]]
    where
      plOfXs = powerList xs

powerF (Pure e) = Spread [Pure e]
powerF (Spread []) = Spread [Pure []]
powerF (Spread (x:xs)) 
  = meltIn x plOfXs
    where
      plOfXs = powerF $ Spread xs

meltIn :: Free a -> Free a -> Free a
meltIn _ (Pure a) = error "Pure value can't be melt into"
meltIn e (Spread list) = Spread $ e:list

--powerF2 :: Eq (Free [a]) => Free [a] -> Free [a]
powerF2 (Pure e) = Spread [Pure e]
powerF2 (Spread []) = Spread [Pure []]
powerF2 (Spread xs) = 
  Spread (map (\xs -> (Spread xs)) (powerList'' xs))



{-
powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = 
  Set (map (\xs -> (Set xs)) (powerList'' xs))

powerList'' [] = [[]]
powerList'' list = [] :  [e:l | e <- list
                              , l <- powerList'' 
                                     $ eliminated e list]
  where
      eliminated :: Eq a => a -> [a] -> [a]
      eliminated e (e':r)
         | e == e'   = r
         | otherwise = eliminated e r
      eliminated _ [] = error "not valid"     
-}

{-
powerFree (Pure e) = Spread [Pure e]
powerFree (Spread []) = Spread []
powerFree (Spread (x:xs)) 
  = Spread $ plOfXs ++ [x:l| l <- plOfXs]
    where
      (Spread plOfXs) = powerFree $ Spread xs
-}

listToFree :: [a] -> Free a
listToFree list = Spread [Pure e | e <- list]

--data Freer sig a = Op (sig (Freer sig a)) | Value a
--data Sig f = Level f | Element f
--newtype Combi a = Freer (Sig Freer) a

--freePower (Pure a) = Spread [a]
--freePower (Spread  list) = listToFree $ powerList'' list  

--freeToList (Pure a) = [a]
--freeToList (Spread list) = [freeToList e| e <- list]


genFree freeA 0 = Pure ()
genFree (Pure p) n = undefined

--gen list 0 = [ [] ]
--gen list n = [ new:old | new <- list, old <- gen list (n-1) ]



----------------------------------------------------------
----------------- Relations represented as Sets

type Rel a = Set (a,a)

-- domR gives the domain of a relation.
domR :: Ord a => Rel a -> Set a
domR (Set r) = list2set [ x | (x,_) <- r ]

-- ranR gives the range of a relation.
ranR :: Ord a => Rel a -> Set a
ranR (Set r) = list2set [ y | (_,y) <- r ]

-- idR creates the identity relation ∆A over a set A:
idR :: Ord a => Set a -> Rel a
idR (Set xs) = Set [(x,x) | x <- xs]

-- The total relation over a set is given by:
totalR :: Set a -> Rel a
totalR (Set xs) = Set [(x,y) | x <- xs, y <- xs ]

-- invR inverts a relation (i.e., the function maps R to R−1
invR :: Ord a => Rel a -> Rel a
invR (Set []) = (Set [])
invR (Set ((x,y):r)) = insertSet (y,x) (invR (Set r))

-- inR checks whether a pair is in a relation.
inR :: Ord a => Rel a -> (a,a) -> Bool
inR r (x,y) = inSet (x,y) r

-- complement of a relation R
complR :: Ord a => Set a -> Rel a -> Rel a
complR (Set xs) r =
  Set [ (x,y) | x <- xs
              , y <- xs
              , not (inR r (x,y))]

-- A check for reflexivity of R
reflR :: Ord a => Set a -> Rel a -> Bool
reflR set r = subSet (idR set) r

-- A check for irreflexivity of R on A p
irreflR :: Ord a => Set a -> Rel a -> Bool
irreflR (Set xs) r =
  all (\ pair -> not (inR r pair)) [(x,x) | x <- xs]

-- A check for symmetry of R
symR :: Ord a => Rel a -> Bool
symR (Set []) = True
symR (Set ((x,y):pairs)) 
  | x == y = symR (Set pairs)
  | otherwise = 
        inSet (y,x) (Set pairs)
        && symR (deleteSet (y,x) (Set pairs))

-- A check for transitivity of R
transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s) = and [ trans pair (Set s) | pair <- s ] 
  where
    trans (x,y) (Set r) =
      and [ inSet (x,v) (Set r) | (u,v) <- r, u == y ]

composePair :: Ord a => (a,a) -> Rel a -> Rel a
composePair (x,y) (Set []) = Set []
composePair (x,y) (Set ((u,v):s))
  | y == u = insertSet (x,v) (composePair (x,y) (Set s))
  | otherwise = composePair (x,y) (Set s)

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set []) set2 = set2
unionSet (Set (x:xs)) set2 =
  insertSet x (unionSet (Set xs) (deleteSet x set2))

compR :: Ord a => Rel a -> Rel a -> Rel a
compR (Set []) _ = (Set [])
compR (Set ((x,y):s)) r =
  unionSet (composePair (x,y) r) (compR (Set s) r)

repeatR :: Ord a => Rel a -> Int -> Rel a
repeatR r n 
  | n < 1 = error "argument < 1"
  | n == 1 = r
  | otherwise = compR r (repeatR r (n-1))

r = Set [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
r2 = compR r r
r3 = repeatR r 3
r4 = repeatR r 4


