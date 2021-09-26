module Arrows where

--- https://www.haskell.org/arrows/
{-
class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b,d) (c,d)
-}


--- 10.1 Notions of Computation

add :: (b -> Int) -> (b -> Int) -> (b -> Int)
add f g b = f b + g b

--- one of many possible state handling
type State r i o = (r,i) -> (r,o)

addST :: State r b Int -> State r b Int -> State r b Int
addST f g (s,b) = 
  let (s',x) = f (s,b)
      (s'',y) = g (s',b)
      in (s'', x+y)

--- non det search
type NonDet i o = i -> [o]

addND :: NonDet b Int -> NonDet b Int -> NonDet b Int
addND f g b = [x+y | x <- f b, y <- g b]

--- bahaviour transformer
type MapTrans r i o = (r -> i) -> (r -> o)

addMT :: MapTrans r b Int -> MapTrans r b Int -> MapTrans r b Int
addMT f g m s = f m s + g m s

--- simple automata
newtype Auto i o = A (i -> (o, Auto i o))

addAUTO :: Auto b Int -> Auto b Int -> Auto b Int
addAUTO (A f) (A g) 
  = A (\b -> let 
              (x,f') = f b
              (y,g') = g b
            in (x+y, addAUTO f' g'))

class Categroy a where
  idA :: a b b
  (>>>) :: a b c -> a c d -> a b d

instance Categroy (->) where
  idA = id
  f >>> g = g . f