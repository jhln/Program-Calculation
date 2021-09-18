module SemiringAlgos where

import Data.List (transpose)
import Data.Set hiding (map)
import Prelude 


infixl 9 @.
infixl 8 @+
class Semiring r where
  zero, one :: r
  closure :: r -> r
  (@+), (@.) :: r -> r -> r

instance Semiring Bool where
  zero = False
  one = True
  closure x = True
  (@+) = (||)
  (@.) = (&&)


--- 3. Matrices and reachability

data Matrix a = Scalar a
              | Matrix [[a]]

type BlockMatrix a = (Matrix a, Matrix a,
                      Matrix a, Matrix a)

mjoin :: BlockMatrix a -> Matrix a
mjoin (Matrix a, Matrix b,
       Matrix c, Matrix d) 
  =  Matrix ((a `hcat` b) ++ (c `hcat` d))
  where 
    hcat = zipWith (++)

msplit :: Matrix a -> BlockMatrix a
msplit (Matrix (row:rows)) =
  (Matrix [[first]], Matrix [top],
   Matrix left, Matrix rest) 
   where
     (first:top) = row
     (left, rest) = unzip (map (\(x:xs) -> ([x],xs)) rows)

instance Semiring a => Semiring (Matrix a) where
  zero  = Scalar zero
  one   = Scalar one
  (Scalar a) @+ (Scalar b)      = Scalar (a @+ b)
  (Matrix a) @+ (Matrix b)      = Matrix (zipWith (zipWith (@+)) a b)
  (Scalar s) @+ m               = m @+ (Scalar s)
  (Matrix [[a]]) @+ (Scalar b)  = Matrix [[a @+ b]]
  m @+ s = mjoin (first @+ s, top, left, rest @+ s)
    where 
      (first, top, left, rest) = msplit m
  (Scalar a) @. (Scalar b) = Scalar (a @. b)
  (Scalar a) @. (Matrix b) = Matrix (map (map (a @.)) b)
  (Matrix a) @. (Scalar b) = Matrix (map (map (@. b)) a)
  (Matrix a) @. (Matrix b) = 
    Matrix [[foldl1 (@+) (zipWith (@.) row col) | col <- cols] | row <- a]
      where 
        cols = transpose b
  closure (Matrix [[x]]) = Matrix [[closure x]]
  closure m = mjoin
    (first' @+ top' @. rest' @. left' , top' @. rest', rest' @. left', rest')
    where
      (first, top, left, rest) = msplit m
      first' = closure first
      top' = first' @. top
      left' = left @. first'
      rest' = closure (rest @+ left' @. top)


data ShortestDistance = Distance Int | Unreachable

instance Semiring ShortestDistance where
  zero = Unreachable
  one = Distance 0
  closure x = one
  x @+ Unreachable = x
  Unreachable @+ x = x
  Distance a @+ Distance b = Distance (min a b)
  x @. Unreachable = Unreachable
  Unreachable @. x = Unreachable
  (Distance a) @. (Distance b) = Distance (a + b)


--- 4. Graphs and paths

data ShortestPath n = Path Int [(n,n)] | NoPath

instance Ord n => Semiring (ShortestPath n) where
  zero = NoPath
  one = Path 0 []
  closure x = one

  x @+ NoPath = x
  NoPath @+ x = x
  (Path a p) @+ (Path a' p')
    | a < a'            = Path a p
    | a == a' && p < p' = Path a p
    | otherwise         = Path a' p'
  
  x @. NoPath = NoPath
  NoPath @. x = NoPath
  Path a p @. Path a' p' = Path (a + a') (p ++ p')


data LongestDistance = LDistance Int
                     | LUnreachable
                     | LInfinite

instance Semiring LongestDistance where
  zero = LUnreachable
  one = LDistance 0

  closure LUnreachable = LDistance 0
  closure (LDistance 0) = LDistance 0
  closure _ = LInfinite

  x @+ LUnreachable = x
  LUnreachable @+ x = x
  LInfinite @+ _ = LInfinite
  _ @+ LInfinite = LInfinite
  (LDistance x) @+ (LDistance y) = LDistance (max x y)

  x @. LUnreachable = LUnreachable
  LUnreachable @. x = LUnreachable
  LInfinite @. _ = LInfinite
  _ @. LInfinite = LInfinite
  (LDistance x) @. (LDistance y) = LDistance (x + y)


--- 5. “Linear” equations and regular languages


data FreeSemiring gen =
  Zero
  | One
  | Gen gen
  | Closure (FreeSemiring gen)
  | (FreeSemiring gen) :@+ (FreeSemiring gen)
  | (FreeSemiring gen) :@. (FreeSemiring gen)


instance Semiring (FreeSemiring gen) where
  zero  = Zero
  one   = One
  Zero @+ x = x
  x @+ Zero = x
  x @+ y = x :@+ y
  Zero @. x = Zero
  x @. Zero = Zero
  One @. x = x
  x @. One = x
  x @. y = x :@. y
  closure Zero = One
  closure x = Closure x


--- 6. Dataflow analysis

class Monoid m => CommutativeMonoid m

instance Ord a => CommutativeMonoid (Set a)

newtype Transfer m = Transfer (m -> m)

instance (Eq m, CommutativeMonoid m) => Semiring (Transfer m) where
  zero = Transfer (const mempty)
  one = Transfer id
  (Transfer f) @+ (Transfer g) = Transfer (\x -> f x `mappend` g x)
  (Transfer f) @. (Transfer g) = Transfer (f . g)
  closure (Transfer f) = Transfer (\x -> fixpoint (\y -> x `mappend` f y) x)
    where 
      fixpoint f init = 
        if init == next
          then init
          else fixpoint f next
        where 
          next = f init


--- 7. Polynomials, power series and knapsacks

instance Semiring r => Semiring [r] where
  zero = []
  one = [one]
  [] @+ y = y
  x @+ [] = x
  (x:xs) @+ (y:ys) = (x @+ y):(xs @+ ys)
  [] @. _ = []
  _ @. [] = []
  (a:p) @. (b:q) =  (a @. b) 
                  : (map (a @.) q @+ map (@. b) p @+ (zero:(p @. q)))
  closure [] = one
  closure (a:p) = r
    where r = [closure a] @. (one:(p @. r))


knapsack values maxweight = closure values !! maxweight

--- 8. Linear recurrences and Petri nets