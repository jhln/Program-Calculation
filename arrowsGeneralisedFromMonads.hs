module ArrowsGeneralisedFromMonads where

import Control.Monad

class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b, d) (c, d)

newtype Kleisli m a b = K (a -> m b)

instance Monad m => Arrow (Kleisli m) where
  arr f = K $ \b -> return $ f b
  K f >>> K g = K $ \b -> f b >>= g
  first (K f) = K $ \(b, d) -> f b >>= \c -> return (c, d)


second :: Arrow a => a b c -> a (d, b) (d, c)
second f = arr swap >>> first f >>> arr swap
  where swap (x, y) = (y, x)

(***) :: Arrow a => a b c -> a d e -> a (b, d) (c, e)
f *** g = first f >>> second g

(&&&) :: Arrow a => a b c -> a b d -> a b (c, d)
f &&& g = (arr $ \b -> (b,b)) >>> (f *** g)

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>> (arr $ \(b, c) -> b `op` c)


--- 4.2 Arrows and Interpreters


data Exp 
  = Var String 
  | Add Exp Exp
  | If Exp Exp Exp
  | Lam String Exp 
  | App Exp Exp

data Val 
  = Num Int
  | Bl Bool
  -- | Fun (A Val Val)

type Env 
  = [(String, Val)]

eval' :: MonadFail m => Exp -> Env -> m Val
eval' (Var s) env = 
  case lookup s env of
    Nothing -> fail "var not defined"
    Just x  -> return x
eval' (Add e1 e2) env = liftM2 add (eval' e1 env) (eval' e2 env)
  where 
    add (Num u) (Num v) = Num (u + v)
eval' (If e1 e2 e3) env = eval' e1 env >>= \(Bl b) ->
  if b then eval' e2 env else eval' e3 env


eval :: ArrowChoice a => Exp -> a Env Val
eval (Var s) = arr $ \env -> 
  case lookup s env of
    Nothing   -> Num 0
    (Just x)  -> x
eval (Add e1 e2) = liftA2 add (eval e1) (eval e2)
  where 
    add (Num u) (Num v) = Num (u + v)
{-
eval (If e1 e2 e3) = 
  (eval e1 &&& (eval e2 &&& eval e3)) 
  >>>
  arr (\(Bl b, (v1, v2)) -> if b then v1 else v2)
-}
eval (If e1 e2 e3) = 
  (eval e1 &&& arr id) 
  >>>
  arr (\(Bl b, env) -> if b then Left env else Right env) 
  >>>
  ((eval e2) ||| (eval e3))

--eval (If e1 e2 e3) = 
  --test (eval e1 >>> (arr $ \(Bl b) -> b)) ((eval e2 )||| (eval e3))

test :: Arrow a => a b Bool -> a b (Either b b)
test f = 
  (f &&& arr id) 
  >>> 
  arr (\(b, x) -> if b then Left x else Right x)


class Arrow a => ArrowChoice a where
  left :: a b c -> a (Either b d) (Either c d)

instance Monad m => ArrowChoice (Kleisli m) where
  left (K f ) = 
    K $ \x -> case x of
                Left b -> f b >>= \c -> return (Left c)
                Right d -> return $ Right d

right f = arr mirror >>> left f >>> arr mirror
  where 
    mirror (Left x) = Right x
    mirror (Right x) = Left x

f <+> g = left f >>> right g

f ||| g = (f <+> g) >>> arr untag
  where 
    untag (Left x) = x
    untag (Right y) = y



class Arrow a => ArrowApply a where
  app :: a (a b c, b) c

instance Monad m => ArrowApply (Kleisli m) where
  app = K $ \(K f, x) -> f x