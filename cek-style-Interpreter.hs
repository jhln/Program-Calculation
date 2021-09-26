module CEK_Sytel_Interpreter where

--- https://matt.might.net/articles/cek-machines/
--- https://matt.might.net/articles/cesk-machines/


type Var 
  = String
data Lambda 
  = Var :=> Exp
data Exp 
  = Ref Var
  | Lam Lambda
  | Exp :@ Exp
type State  
  = (Exp,Env,Kont)
data D    -- "denoted values, domain of values"
  = Clo (Lambda, Env)
  | Str String
  | I Integer
  -- more values possible
type Env  
  = Var -> D
data Kont 
  = Mt  -- emty continuation
  | Ar (Exp,Env,Kont)   -- argument continuation
  | Fn (Lambda,Env,Kont)  -- function continuation

step :: State -> State
step (Ref x, ρ, κ) = (Lam lam,ρ',κ) 
   where Clo (lam, ρ') = ρ(x)
step (f :@ e, ρ, κ) 
  = (f, ρ,  Ar(e, ρ, κ))
step (Lam lam, ρ, Ar(e, ρ', κ)) 
  = (e, ρ', Fn(lam, ρ, κ))
step (Lam lam, ρ, Fn(x :=> e, ρ', κ)) 
  = (e, ρ' // [x ==> Clo (lam, ρ)], κ)

(==>) :: a -> b -> (a,b)
(==>) x y = (x,y)
(//) :: Eq a => (a -> b) -> [(a,b)] -> (a -> b)
(//) f [(x,y)] 
  = \ x' -> if x == x'
              then y
              else f x'

terminal :: (State -> State) -> (State -> Bool) -> State -> State
terminal step isFinal s0 
  | isFinal s0 = s0
  | otherwise  = terminal step isFinal (step(s0))

inject :: Program -> State
inject e = (e, ρ0, Mt)
  where 
    ρ0 :: Env
    ρ0 = \ x -> error $ "no binding for " ++ x


type Program = Exp

isFinal :: State -> Bool
isFinal (Lam _, ρ, Mt) = True
isFinal _              = False

evaluate :: Program -> State
evaluate pr = terminal step isFinal $ inject pr




