{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module MonadicStyleBacktracking where

import Prelude hiding ((++), Maybe, Just, Nothing, maybe)


class (Monad m) => MonadZero m where
  zero :: m a
class ( MonadZero m ) => MonadPlus m where
 (++) :: m a -> m a -> m a

class (Monad m) => MonadState st m where
  update :: ( st -> st ) -> m st

class (MonadZero m) => MonadParser tok m where
  item :: m tok

class Run m n where
  run :: m a -> n a
instance Run m m where
  run = id


when, unless :: (Monad m) => Bool -> m () -> m ()
when b m = 
  if b 
    then m 
    else return ()
unless b m = 
  if b
    then return () 
    else m

guard :: (MonadZero m) => Bool -> m ()
guard b = 
  if b
    then return () 
    else zero

(|>) :: (MonadZero m) => m a -> (a -> Bool) -> m a
m |> p = do 
    a <- m 
    if p a
      then return a
      else zero

opt :: (MonadPlus m) => m a -> a -> m a
opt m a = m ++ return a

many :: (MonadPlus m) => m a -> m [a]
many m = ms
  where 
    ms = do 
          a <- m
          x <- ms
          return (a : x)
        ++ 
        return []

sepBy :: (MonadPlus m) => m a -> m b -> m [a]
sepBy m s = do 
    a <- m
    x <- many ( do s ; m )
    return (a : x)

accepts :: (MonadZero m, MonadState [tok] m) => m a -> [tok] -> m a
accepts p s = do 
  old <- update (\_ -> s)
  a <- p
  t <- update (\_ -> old)
  guard $ null $ t `asTypeOf` s
  return a


----- 2. Apllications

----- 2.1 Backtracking


person :: ( MonadPlus m ) => m String
person = return "abraham" 
      ++ return "lot" 
      ++ return "milcah" 
      ++ return "nachor"
      ++ return "sarah" 
      ++ return "terach" 
      ++ return "yiscah"

child :: ( MonadPlus m ) => String -> m String
child "terach" = return "abraham" ++ return "nachor" ++ return "haran"
child "abraham" = return "isaac"
child "haran" = return "lot" ++ return "milcah" ++ return "yiscah"
child "sarah" = return "isaac"
child _ = zero

listAllP :: (MonadPlus m ) => m String
listAllP = do p <- person ; return p

listAllPC :: (MonadPlus m ) => m (String, String)
listAllPC = do p <- person ; c <- child p ; return ( p , c )

descendant :: ( MonadPlus m ) => String -> m String
descendant = transitiveClosure child

transitiveClosure :: ( MonadPlus m ) => ( a -> m a ) -> ( a -> m a )
transitiveClosure m = m +++ ( transitiveClosure m `at` m )

at :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f `at` g = \ a -> g a >>= f

(+++) :: (MonadPlus m) => (a -> m b) -> (a -> m b) -> (a -> m b)
f +++ g = \ a -> (f a ++ g a)

transitiveClosure' :: (MonadPlus m) => (a -> m a) -> (a -> m a)
transitiveClosure' m = ( return +++ transitiveClosure' m ) `at` m

query1 = run ( descendant "terach" :: BacktrML String ) :: [] String



----- 8 queens problem

queens :: (MonadPlus m) => Int -> m [Int]
queens n = place n [1 .. n] [] []

place :: ( MonadPlus m ) => Int -> [Int] -> [Int] -> [Int] -> m [Int]
place 0 rs d1 d2 = return []
place i rs d1 d2 = do 
                    (q, rs') <- select rs
                    let q1 = q - i
                    let q2 = q + i
                    guard (q1 `notElem` d1)
                    guard (q2 `notElem` d2)
                    qs <- place (i - 1) rs' (q1 : d1) (q2 : d2)
                    return ( q : qs )

select :: (MonadPlus m) => [a] -> m (a, [a])
select [] = zero
select (a : x) = 
  return (a, x)
  ++ 
  do 
    (b, x') <- select x
    return (b, a:x')


-- query2 = length (run (queens 8 :: BacktrM [Int]) :: [] [Int])




----- 2.2 Parsing


instance (MonadZero m, MonadState [tok] m) => MonadParser tok m where
  item = update tail >>= 
          \x -> case x of 
                  []    -> zero 
                  (a:_) -> return a



data Term 
  = Var String
  | Fun String [Term]


term :: (MonadPlus m, MonadParser Char m) => m Term
term = do 
        v <- var
        return ( Var v )
      ++ 
        do 
          f <- fun
          as <- opt args []
          return $ Fun f as


args :: (MonadPlus m, MonadParser Char m) => m [Term]
args = do 
  lit '('
  as <- sepBy term ( lit ',' )
  lit ')'
  return as


lit :: (MonadPlus m, MonadParser Char m) => Char -> m Char
lit a = lexical $ item |> (== a )


var, fun :: (MonadPlus m, MonadParser Char m) => m String
var = lexical $
  do 
    a <- upper
    x <- many alpha
    return (a : x)
fun = lexical $
  do 
    a <- lower
    x <- many alpha
    return (a : x)


lexical :: (MonadPlus m, MonadParser Char m) => m a -> m a
lexical p = do 
  a <- p 
  many white
  return a


lower, upper, alpha, white :: (MonadParser Char m) => m Char
lower = item |> isLower
upper = item |> isUpper
alpha = item |> isAlpha
white = item |> isSpace


isLower :: Char -> Bool
isLower = undefined
isUpper :: Char -> Bool
isUpper = undefined
isAlpha :: Char -> Bool
isAlpha = undefined
isSpace :: Char -> Bool
isSpace = undefined


-- parse :: String -> Maybe Term
-- parse s = run $ accepts ( do many white ; term ) s :: ParserM Char Term 



------ 3. Transformers

------ 3.1 Monad Transformers


class (Monad m,  Monad (t m)) => MonadT t m where
  up :: m a -> t m a
  down :: t m a -> m a

instance (Monad (t m), MonadZero m, MonadT t m) => MonadZero (t m) where
  zero = up zero

instance (MonadState st m, MonadT t m) => MonadState st ( t m ) where
  update = up . update

instance (MonadT t m, Run m n) => Run (t m) n where
  run = run . down




------ 3.2 State Monad Transformers

data StateT st m a = StateT { runStateT :: st -> m (st,a) }

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(s', a) -> (s', f a)) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (s, a)

instance (Monad m) => Monad (StateT st m) where
  m >>= k = StateT $ \s ->  do
        (s', a) <- runStateT m s  
        runStateT (k a) s'
  return a = StateT $ \ s -> return (s, a)

instance (Monad m) => MonadState st (StateT st m) where
  update st = StateT $ \s -> return (st s, s)


instance (Monad m ) => MonadT ( StateT st ) m where
  up m = StateT $ \s -> do
                          a <- m
                          return (s, a)
  down m =  undefined -- m >>= \(st, a) -> return a
    

{-
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    ~(_, s') <- runStateT m s
    return s'-}

--- irgendwie kann man hier nur eine machen :(

--instance (MonadZero m) => MonadZero (StateT st m) where
  --zero = StateT $ \s -> zero
instance (MonadPlus m) => MonadPlus (StateT st m) where
  (StateT m) ++ (StateT n) = StateT $ \s -> (m s ++ n s)




------ 3.3 Maybe Monad

data Maybe a 
  = Nothing 
  | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x


instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just $  f x
instance Applicative Maybe where
  pure f = Just f
instance Monad Maybe where
  m >>= k = maybe zero k m
  return = Just
instance MonadZero Maybe where
  zero = Nothing
instance MonadPlus Maybe where
  m ++ n = maybe n Just m

type ParserM tok = StateT [tok] Maybe
type BacktrM = Maybe -- does not work



------ 3.3 List Monad

list :: b -> (a -> b -> b) -> [a] -> b
list e (*) [] = e
list e (*) (a:as) = a * list e (*) as

instance MonadZero [] where
  zero = []
instance MonadPlus [] where
  m ++ n = list n (:) m

type BacktrML = []
type ParserML tok = StateT [tok] []



------- 4. Efficient backtracking monads

data Tree a 
  = Empty 
  | Node (Tree a) a (Tree a)


data EndoT m a = EndoT {proj :: m a -> m a}

{-
--type EndoT m a = m a -> m a

instance (MonadPlus m) => Monad (EndoT m) where
  m >>= k = \f -> (m zero >>= \a -> k a zero) ++ f
  return a = \f -> return a ++ f

instance (MonadPlus m) => MonadT EndoT m where
  up m = \f -> m ++ f
  down m = m zero

--instance MonadZero (EndoT m) where
--  zero = id
instance MonadPlus (EndoT m) where
  m ++ n = m . n
-}


type ContT m a = forall ans . ( a -> m ans ) -> m ans
{-
instance (Monad m) => Monad (ContT m) where
  m >>= k = \f -> m (\a -> k a f)
  return a = \f -> f a
-}


