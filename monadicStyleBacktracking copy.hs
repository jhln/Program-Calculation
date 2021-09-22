{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadicStyleBacktracking where

--import Control.Monad



----- 2. Apllications

----- 2.1 Backtracking


person :: ( MonadPlus m ) => m String
person = return "abraham" `mplus` return "lot" 
      `mplus` return "milcah" 
      `mplus` return "nachor"
      `mplus` return "sarah" 
      `mplus` return "terach" 
      `mplus` return "yiscah"

child :: ( MonadPlus m ) => String -> m String
child "terach" = return "abraham" `mplus` return "nachor" `mplus` return "haran"
child "abraham" = return "isaac"
child "haran" = return "lot" `mplus` return "milcah" `mplus` return "yiscah"
child "sarah" = return "isaac"
child _ = mzero

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
f +++ g = \ a -> (f a `mplus` g a)

transitiveClosure' :: (MonadPlus m) => (a -> m a) -> (a -> m a)
transitiveClosure' m = ( return +++ transitiveClosure' m ) `at` m

-- query1 = run ( descendant "terach" :: BacktrM String ) :: [] String




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
select [] = mzero
select (a : x) = 
  return (a, x)
  `mplus` 
  do 
    (b, x') <- select x
    return (b, a:x')


-- query2 = length (run (queens 8 :: BacktrM [Int]) :: [] [Int])




----- 2.2 Parsing

class (Monad m) => MonadZero m where
  zero :: m a
class ( MonadZero m ) => MonadPlus m where
 (++) :: m a -> m a -> m a

class (Monad m) => MonadState st m where
  update :: ( st -> st ) -> m st

class (MonadZero m) => MonadParser tok m where
  item :: m tok

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
      `mplus` 
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

lower, upper, alpha, white :: (MonadParser Char m) => m Char
lower = item |> isLower
upper = item |> isUpper
alpha = item |> isAlpha
white = item |> isSpace

-- parse :: String -> Maybe Term
-- parse s = run $ accepts ( do many white ; term ) s :: ParserM Char Term 