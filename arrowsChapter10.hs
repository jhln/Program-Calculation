module ArrowsChapter10 where

--- https://www.haskell.org/arrows/

import Prelude hiding (pure, Either, Left, Right)

--- 10.1 Notions of Computation

add :: (b -> Int) -> (b -> Int) -> (b -> Int)
add f g b = f b + g b

{-

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

-}

{-
class Categroy a where
  idA :: a b b
  (>>>) :: a b c -> a c d -> a b d

instance Categroy (->) where
  idA = id
  f >>> g = g . f
-}

(.&.) :: (a -> a') -> (b -> b') -> (a,b) -> (a',b')
(f .&. g) (a,b) = (f a, g b)



class Arrow a where
  pure :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b,d) (c,d)

idA :: Arrow a => a b b
idA = pure id

second :: Arrow a => a b c -> a (d, b) (d,c)
second f = pure swap >>> first f >>> pure swap

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)


instance Arrow (->) where
  pure f = f
  f >>> g = g . f
  first f = f .&. id


newtype State r i o = ST ((r,i) -> (r,o))
newtype NonDet i o = ND (i -> [o])
newtype MapTrans r i o  = MT ((r -> i) -> (r -> o))
newtype Auto i o = A (i -> (o, Auto i o))



instance Arrow (State r) where
  pure f = ST (id .&. f)
  ST f >>> ST g = ST (g . f)
  first (ST f) = ST (assoc . (f .&. id) . unassoc)

assoc :: ((a,b),y) -> (a,(b,y))
assoc ((a,b),c) = (a,(b,c))
unassoc :: (a,(b,y)) -> ((a,b),y)
unassoc (a,(b,c)) = ((a,b),c)

instance Arrow NonDet where
  pure f = ND $ \b -> [f b]
  ND f >>> ND g = ND $ \b -> [d | c <- f b, d <- g c]
  first (ND f) = ND $ \(b,d) -> [(c,d) | c <- f b]

instance Arrow (MapTrans r) where
  pure f = MT (f .)
  MT f >>> MT g = MT (g . f)
  first (MT f) = MT (zipMap . (f .&. id) . unzipMap)

zipMap :: (r -> a, r -> b) -> (r -> (a,b))
zipMap h s = (fst h s, snd h s)
unzipMap :: (r -> (a,b)) -> (r -> a, r -> b)
unzipMap h = (fst . h, snd . h)

instance Arrow Auto where
  pure f = A $ \b -> (f b , pure f)
  A f >>> A g = A $ \b -> let 
                            (c, f') = f b
                            (d, g') = g c
                          in (d,f' >>> g')
  first (A f) = A $ \(b,d) -> let 
                                (c,f') = f b
                              in ((c,d), first f')

(***) :: Arrow a => a b c -> a b' c' -> a (b,b') (c,c')
f *** g = first f >>> second g

(&&&) :: Arrow a => a b c -> a b c' -> a b (c,c')
f &&& g = pure dup >>> (f *** g)
  where
    dup b = (b,b)

addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = f &&& g >>> (pure $ uncurry (+))


------ 10.2 Special cases and facilities


curryA :: Arrow a => a (b,c) d -> b -> a c d
curryA f b = mkPair b >>> f

mkPair :: Arrow a => b -> a c (b,c)
mkPair b = pure $ \c -> (b,c)


--- Monads
class Arrow a => ArrowApply a where
  app :: a (a c d, c) d

instance ArrowApply (->) where
  app (f,c) = f c

--- Conditionals
data Either a b  
  = Left a
  | Right b

(.+.) :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
(f .+. g) (Left a) = Left $ f a
(f .+. g) (Right b) = Right $ g b


class Arrow a => ArrowChoice a where
  left :: a b c -> a (Either b d) (Either c d)

right :: ArrowChoice a => a b c -> a (Either d b) (Either d c)
right f = pure mirror >>> left f >>> pure mirror
  where
    mirror (Left x) = Right x
    mirror (Right y) = Left y

assocsum :: Either (Either a b ) c -> Either a (Either b c)
assocsum (Left (Left a)) = Left a
assocsum (Left (Right b)) = Right $ Left b
assocsum (Right c) = Right $ Right c

distr :: (Either a b, c) -> Either (a,c) (b,c)
distr (Left a, c) = Left (a,c)
distr (Right b,c) = Right (b,c)

(<+>) :: ArrowChoice a => a b c -> a b' c' -> a (Either b b') (Either c c')
f <+> g = left f >>> right g

(|||) :: ArrowChoice a => a b d -> a c d -> a (Either b c) d
f ||| g = f <+> g >>> pure untag
  where 
    untag (Left x) = x
    untag (Right y) = y


instance ArrowChoice (->) where
  left f = f .+. id

instance ArrowChoice Auto where
  left (A f) = A lf
    where
      lf (Left b) = let (c,f') = f b
                    in (Left c, left f')
      lf (Right d) = (Right d, left $ A f)


--- Feedback

trace :: ((b,d) -> (c,d)) -> b -> c
trace f b = let (c,d) = f (b,d) in c


class Arrow a => ArrowLoop a where
  loop :: a (b,d) (c,d) -> a b c


instance ArrowLoop (->) where
  loop = trace


instance ArrowLoop (State r) where
  loop (ST f) = ST $ trace $ unassoc . f . assoc

instance ArrowLoop (MapTrans r) where
  loop (MT f) = MT $ trace $ unzipMap . f . zipMap

instance ArrowLoop Auto where
  loop (A f) = A $ \b -> let
                          ((c,d), f') = f (b,d)
                         in (c, loop f')


--- 10.3 Arrow notation


