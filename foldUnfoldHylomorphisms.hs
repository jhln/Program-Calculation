module OrigamiProgramming where

import Prelude hiding (Nothing, Just, Maybe)


-------- 2. Sorting


data List a 
  = Nil 
  | Cons a (List a)
  deriving Show

wrap :: a -> List a
wrap x = Cons x Nil

nil :: List a -> Bool
nil Nil = True
nil (Cons _ _) = False

foldL :: (a -> b -> b) -> b -> List a -> b
foldL f e Nil = e
foldL f e (Cons x xs) = f x $ foldL f e xs

isort :: Ord a => List a -> List a
isort = foldL insert Nil
  where
    insert :: Ord a => a -> List a -> List a
    insert y Nil = wrap y
    insert y (Cons x xs)
      | y < x = Cons y $ Cons x xs
      | otherwise = Cons x $ insert y xs

data Maybe a 
  = Just a
  | Nothing
  deriving Show

unfoldL' :: (b -> Maybe (a,b)) -> b -> List a
unfoldL' f u = case f u of
                Nothing -> Nil
                Just (x,v) -> Cons x $ unfoldL' f v

unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f g b = if p b
                    then Nil
                    else Cons (f b) $ unfoldL p f g $ g b

foldL' :: (Maybe (a,b) -> b) -> List a -> b
foldL' f Nil = f Nothing
foldL' f (Cons x xs) = f (Just (x, foldL' f xs))

delmin :: Ord a => List a -> Maybe (a, List a)
delmin Nil = Nothing
delmin xs = Just (y, deleteL y xs)
  where
    y = minimumL xs

minimumL :: Ord a => List a -> a
minimumL (Cons x xs) = foldL min x xs

deleteL :: Eq a => a -> List a -> List a
deleteL y Nil = Nil
deleteL y (Cons x xs)
  | y == x = xs
  | otherwise = Cons x $ deleteL y xs

ssort :: Ord a => List a -> List a
ssort = unfoldL' delmin

bubble :: Ord a => List a -> Maybe (a,List a)
bubble = foldL step Nothing 
  where 
    step x Nothing = Just (x, Nil)
    step x (Just (y,ys))
      | x < y = Just (x, Cons y ys)
      | otherwise = Just (y, Cons x ys)

bsort :: Ord a => List a -> List a
bsort = unfoldL' bubble



--------------- Hylomorphisms

factorial = foldL (*) 1 . unfoldL (== 0) id pred

hyloL f e p g h = foldL f e . unfoldL p g h

factorial' = hyloL (*) 1 (== 0) id pred

hyloL' f e p g h b = 
  if p b
    then e
    else f (g b) $ hyloL' f e p g h $ h b

factorial'' n = 
  if n == 0
    then 1
    else n * (factorial'' $ pred n)


-------- 3. Loops

data Nat 
  = Zero 
  | Succ Nat
  deriving Show

toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Succ $ toNat $ n-1

foldN :: a -> (a -> a) -> Nat -> a
foldN z s Zero = z
foldN z s (Succ n) = s $ foldN z s n

iter :: Nat -> (a -> a) -> (a -> a)
iter n f x = foldN x f n

predN :: Nat -> Maybe Nat
predN Zero = Nothing
predN (Succ n) = Just n

predN' :: Nat -> Maybe Nat
predN' = foldN Nothing sub1 
  where
    sub1 :: Maybe Nat -> Maybe Nat
    sub1 Nothing = Just Zero
    sub1 (Just n) = Just $ Succ n

subN :: Nat -> Nat -> Maybe Nat
subN a b = foldN (Just a) predNlifted b
  where
    predNlifted :: Maybe Nat -> Maybe Nat
    predNlifted Nothing = Nothing
    predNlifted (Just n) = predN' n


unfoldN' :: (a -> Maybe a) -> a -> Nat
unfoldN' f x = 
  case f x of
    Nothing   -> Zero
    Just y    -> Succ $ unfoldN' f y


----- minimiation function from primitive recursion
unfoldN :: (a -> Bool) -> (a -> a) -> a -> Nat
unfoldN p f x =
  if p x
    then Zero
    else Succ $ unfoldN p f $ f x


----- beyond primitive recursion


untilN :: (a -> Bool) -> (a -> a) -> a -> a
untilN p f x = foldN x f $ unfoldN p f x



------ 4. Traversals

data Rose a = Node a (Forest a)
type Forest a = List (Rose a)

foldR :: (a -> y -> b) -> (List b -> y) -> Rose a -> b
foldR f g (Node a ts) = f a $ foldF f g ts

foldF :: (a -> y -> b) -> (List b -> y) -> Forest a -> y
foldF f g ts = g $ mapL (foldR f g) ts

mapL :: (a -> b) -> List a -> List b
mapL f = foldL (app f) Nil
  where
    app :: (a -> b) -> a -> List b -> List b
    app f a bs = Cons (f a) bs 


unfoldR :: (b -> a) -> (b -> List b) -> b -> Rose a
unfoldR f g x = Node (f x) $ unfoldF f g x

unfoldF :: (b -> a) -> (b -> List b) -> b -> Forest a
unfoldF f g x = mapL (unfoldR f g) (g x)


root :: Rose a -> a
root (Node a ts) = a

kids :: Rose a -> Forest a
kids (Node a ts) = ts


--- Depth-first traversal

dft :: Rose a -> List a
dff :: Forest a -> List a
(dft,dff) = (foldR f g, foldF f g)
  where
    f = Cons
    g = concatL

concatL :: List (List a) -> List a
concatL = foldL appendL Nil
appendL :: List a -> List a -> List a
appendL as end = foldL Cons end as


---- level-order traversal

levelt :: Rose a -> List (List a)
levelf :: Forest a -> List (List a)
(levelt, levelf) = (foldR f g, foldF f g)
  where
    f x xss = Cons (wrap x) xss
    g       = foldL (lzw appendL) Nil


lzw :: (a -> a -> a) -> List a -> List a -> List a
lzw f Nil ys = ys
lzw f xs Nil = xs
lzw f (Cons x xs) (Cons y ys) = Cons (f x y) (lzw f xs ys)


bft :: Rose a -> List a
bft = concatL . levelt

bff :: Forest a -> List a
bff = concatL . levelf


---- 5. Shell Sort and Radix Sort


