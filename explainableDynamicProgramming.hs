module ExplainableDynamicProgramming where

-- https://github.com/prashant007/XDP
-- http://hackage.haskell.org/package/DP


import Data.Sort
import Data.Ord
import Data.Function
import Control.Monad.State

type Profile = [Int]
type Distance = [Int]
type Score = Int

dist :: Profile -> Profile -> Distance
dist p = map abs . zipWith (-) p

score :: Profile -> Profile -> Score
score p = sum . dist p

matches :: Profile -> [Profile] -> [(Score,Profile)]
matches p ps = sort [(score p q,q) | q <- ps]

best :: Profile -> [Profile] -> (Score,Profile)
best p = head . matches p

buddies :: [Profile]
buddies = [alice,bob,carol]

alice = [2,2,7]
bob = [3,1,2]
carol = [3,4,2]
dan = [2,2,3]

bestMatchForDan = best dan buddies


---- imporved for single dist comparison


matches' :: Profile -> [Profile] -> [(Distance, Profile)]
matches' p ps = sortBy (compare `on` sum.fst) [(dist p q,q) | q <- ps]

best' :: Profile -> [Profile] -> (Distance,Profile)
best' p = head . matches' p

bestMatchForDan' = best' dan buddies


delta :: Distance -> Distance -> Distance
delta = zipWith (-)

danDistanceMatch = (delta `on` (dist dan)) alice bob

------ larger profiles

aliceL = [6,2,7,8,9,9,5,4,6,2]
bobL = [2,3,6,7,2,2,4,8,7,4]
danL = [6,5,6,8,8,1,9,9,5,7]

danLDistanceMatch = (delta `on` (dist danL)) aliceL bobL


-------- 4. Semiring framework


fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)


type Table = [(Integer,Integer)]

fibS :: Integer -> State Table Integer
fibS 0 = return 0
fibS 1 = return 1
fibS n = do 
          t <- get
          case lookup n t of
            Just f -> return f
            Nothing -> do 
                        f1 <- fibS (n-1)
                        f2 <- fibS (n-2)
                        let f = f1+f2
                        modify $ \t -> (n,f):t
                        return f

fib' :: Integer -> Integer
fib' n = fst (runState (fibS n) [])

class Semiring a where
  zero, one :: a
  (<+>), (<.>) :: a -> a -> a

ssum :: Semiring a => [a] -> a
ssum = foldr (<+>) zero

instance Semiring Integer where
  zero = 0
  one = 1
  (<+>) = (+)
  (<.>) = (*)

{-
fibT :: DP Integer Integer
fibT 0 = zero
fibT 1 = one
fibT n = memo (n-1) <+> memo (n-2)

fibDT :: Integer -> Integer
fibDT n = runDP fibT n
-}