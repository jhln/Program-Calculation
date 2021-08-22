{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module RelationAlgebaAdjunctions where

import qualified Prelude as P --hiding (Monoid, null)
import qualified Data.Array as A
import Data.Word
import Data.Bifunctor as As

data Bag' a = EmptyB | UnionB (Bag' a) (Bag' a) | SingleB a

instance P.Functor Bag' where
  fmap f EmptyB = EmptyB
  fmap f (UnionB b1 b2) = UnionB (P.fmap f b1) (P.fmap f b2)
  fmap f (SingleB e) = SingleB (f e)


--single a :: a -> U (Free A)
--single 


----------------------------------------------------
-- B    Prototype Implementation
-- B.1  Bags

newtype Bag a = Bag {elements :: [a]}
  deriving ( P.Functor, P.Applicative, P.Monad )

nullBag :: Bag a
nullBag = Bag []

uniteBag :: Bag a -> Bag a -> Bag a
uniteBag b1 b2 = Bag (elements b1 P.++ elements b2)

class Monoid m where
  eps :: m
  act :: m -> m -> m
-- act should be associative and eta its neutral element

class Monoid m => CMonoid m
-- act should be commutative

instance Monoid (Bag a) where
  eps = nullBag
  act = uniteBag
instance CMonoid (Bag a)


------- Aggregation
reduceBag :: CMonoid m => Bag m -> m
reduceBag = P.foldr act eps P.. elements


------- Selection
filter' :: (a -> P.Bool) -> Bag a -> Bag a
filter' p = reduceBag P.. P.fmap (guard p)
  where 
    guard p a = if p a
                  then P.return a
                  else nullBag


-------------------------------------------------
-- B.3    Pointed Sets


class Pointed a where
  null :: a
  isNull :: a -> P.Bool

instance Pointed () where
  null = ()
  isNull () = P.True

instance (Pointed a, Pointed b) => Pointed (a,b) where
  null = (null, null)
  isNull (a,b) = isNull a P.&& isNull b

instance Pointed (Bag a) where
  null = nullBag
  isNull = P.null P.. elements

class (P.Functor (Map k)) => Key k where
  data Map k :: (* -> *)
  empty :: (Pointed v) => Map k v
  isEmpty :: (Pointed v) => Map k v -> P.Bool
  single :: (Pointed v) => (k,v) -> Map k v
  merge :: (Map k v1, Map k v2) -> Map k (v1,v2)
  merge' :: Map k (v1,v2) -> (Map k v1, Map k v2)
  merge' x = (P.fmap P.fst x, P.fmap P.snd x)
  dom :: (Pointed v) => Map k v -> Bag k
  cod :: (Pointed v) => Map k v -> Bag v
  cod t = reduce (P.fmap P.return t)
  lookup :: Map k v -> (k -> v)
  ix :: Bag (k,v) -> Map k (Bag v)
  ix' :: Map k (Bag v) -> Bag (k,v)
  reduce :: (Pointed v, CMonoid v) => Map k v -> v
  reduce = reduceBag P.. cod


instance Key () where -- unit type
  newtype Map () v = Lone v
  empty = Lone null
  isEmpty (Lone v) = isNull v
  single ((),v) = Lone v
  merge (Lone v1, Lone v2) = Lone (v1, v2)
  dom (Lone v) = if P.not P.$ isNull v
                  then P.return ()
                  else eps
  cod (Lone v) = if P.not P.$ isNull v
                  then P.return v
                  else eps
  lookup (Lone v) () = v
  ix kvs = Lone (P.fmap P.snd kvs)
  ix' (Lone vs) = P.fmap (\v -> ((),v)) vs
instance P.Functor (Map ()) where
  fmap f (Lone v) = Lone (f v)



instance Key Word16 where -- constant type
  newtype Map Word16 v = A (A.Array Word16 v)
  empty = A (A.accumArray (P.curry P.snd) null (0,(2 P.^16) P.-1) [])
  isEmpty (A a) = P.all isNull (A.elems a)
  single (k,v) = 
    A (A.accumArray (P.curry P.snd) null (0,(2 P.^16) P.-1) [(k,v)])
  merge (A a, A b) = 
    A (A.listArray (0, (2 P.^16) P.-1) (P.zip (A.elems a) (A.elems b)))
  dom (A a) = Bag [k | (k,v) <- A.assocs a, P.not (isNull v)]
  cod (A a) = Bag [v | (k,v) <- A.assocs a, P.not (isNull v)]
  lookup (A a) k = a A.! k
  ix kvs = 
    A (A.accumArray (\xs x -> Bag (x:elements xs)) 
                  null 
                  (0,(2 P.^16) P.-1) 
                  (elements kvs))
  ix' (A a) = Bag [(k,v) | (k,vs) <- A.assocs a, v <- elements vs]
instance P.Functor (Map Word16) where
  fmap f (A a) = A (P.fmap f a)


instance (Key k, Pointed v) => Pointed (Map k v) where
  null = empty
  isNull = isEmpty


instance (Key k1, Key k2) => Key (P.Either k1 k2) where -- sum types
  newtype Map (P.Either k1 k2) v = Pair (Map k1 v, Map k2 v)
  empty = Pair (empty, empty)
  isEmpty (Pair (t1, t2)) = isEmpty t1 P.&& isEmpty t2
  single (P.Left k1, v) = Pair (single (k1, v), empty)
  single (P.Right k2, v) = Pair (empty, single (k2, v))
  merge (Pair (t1,t2), Pair (u1,u2)) 
    = Pair (merge (t1,u1), merge (t2,u2))
  dom (Pair (t1, t2)) 
    = act (P.fmap P.Left (dom t1)) (P.fmap P.Left (dom t1))
  cod (Pair (t1, t2)) = act (cod t1) (cod t2)
  lookup (Pair (t1, t2)) (P.Left k1) = lookup t1 k1
  lookup (Pair (t1, t2)) (P.Right k2) = lookup t2 k2
  ix kvs = Pair (ix (P.fmap (\(P.Left k1, v) -> (k1,v)) kvs),
                 ix (P.fmap (\(P.Right k2, v) -> (k2,v)) kvs))
  ix' (Pair (t1,t2)) 
    = act (P.fmap (\(k1,v) -> (P.Left k1,v)) (ix' t1)) 
          (P.fmap (\(k2,v) -> (P.Right k2,v)) (ix' t2))
instance (P.Functor (Map k1), P.Functor (Map k2)) 
          => P.Functor (Map (P.Either k1 k2)) 
  where
    fmap f (Pair (t1,t2)) = Pair (P.fmap f t1, P.fmap f t2)



instance (Key k1, Key k2) => Key (k1, k2) where -- product types
  newtype Map (k1, k2) v = Comp (Map k1 (Map k2 v))
  empty = Comp empty
  isEmpty (Comp t) = isEmpty t
  single ((k1, k2), v) = Comp (single (k1, single (k2,v)))
  merge (Comp t1, Comp t2) = Comp (P.fmap merge (merge (t1,t2)))
  dom (Comp t) = ix' (P.fmap dom t)
  cod (Comp t) = reduce (P.fmap cod t)
  lookup (Comp t) (k1,k2) = lookup (lookup t k1) k2
  ix kvs = Comp (P.fmap ix (ix (P.fmap _assoc kvs)))
  --  where assoc (Bag ((k1,k2), v)) = Bag (k1, (k2, v))
  -- assoc channged to assocs
  ix' (Comp t) = P.fmap _assoc' (ix' (P.fmap ix' t))
  -- assoc' channged to assocs
instance (P.Functor (Map k1), P.Functor (Map k2)) 
          => P.Functor (Map (k1, k2)) 
  where
    fmap f (Comp t) = Comp (P.fmap (P.fmap f) t)


type Identfier = P.Int
type Name = P.String
type Date = P.Int
type Amount = P.Int

data Customer = 
  C { cid :: Identfier, 
      name :: Name}
data Invoice = 
  I { id :: Identfier, 
      cust :: Identfier, 
      due :: Date, 
      amount :: Amount}

today :: Date
today = 20160919

customers :: Bag Customer
customers = Bag [C 101 "sam",
                C 102 "max",
                C 103 "pat"]

invoices :: Bag Invoice
invoices = Bag [I 201 101 20160921 20,
                I 202 101 20160316 15,
                I 203 103 20160520 10]

indexBy s f = ix (P.fmap (_and f P.id) s)

example :: Bag Customer -> Bag Invoice -> Bag (Bag Name, Bag Amount)
example cs is =
  P.fmap (pair (P.fmap name, P.fmap amount))
         (cod (P.fmap (pair (P.id, 
                               filter' ((P.< today) P.. due)))
                      (merge (indexBy cs cid,
                              indexBy is cust))
              ))
  where
    pair (f,g) (a,b) = (f a, g b)

query :: Bag (P.String, P.Int)
query = reduceBag (P.fmap cp (example customers invoices))
  where
    cp (Bag x, Bag y) = Bag (P.zip x y)