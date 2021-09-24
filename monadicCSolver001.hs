{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module MonadicSolver001  where

import Control.Monad.State.Lazy
import Data.Maybe (fromJust,isJust)
import Prelude hiding (lookup, null)
import Data.Map ((!), Map)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map


trace = flip const


class Monad solver => Solver solver where
  -- the constraints
  type Constraint solver :: *
  -- the terms
  type Term solver :: *
  -- the labels
  type Label solver :: *
  -- produce a fresh constraint variable
  newvarSM :: solver (Term solver)
  -- add a constraint to the current state, and
  -- return whethe the resulting state is consistent
  addSM :: Constraint solver -> solver Bool
  -- reify the current state
  storeSM :: solver [Constraint solver]
  -- run a computation
  runSM :: solver a -> a
  -- mark the current state, and return its label
  markSM :: solver (Label solver)
  -- go to the state with given label
  gotoSM :: Label solver -> solver ()


instance Solver FD where
  type Constraint FD  = FD_Constraint
  type Term       FD  = FD_Term
  type Label      FD  = FDState
  newvarSM 	= newVar () >>= return . FD_Var 
  addSM    	= addFD
  storeSM  	= undefined
  runSM p   	= runFD p
  markSM	= get
  gotoSM	= put



data FD_Constraint where
  FD_Diff :: FD_Term -> FD_Term -> FD_Constraint
  FD_Same :: FD_Term -> FD_Term -> FD_Constraint
  FD_Less :: FD_Term  -> FD_Term -> FD_Constraint
  FD_LT   :: FD_Term -> Int -> FD_Constraint
  FD_GT   :: FD_Term -> Int -> FD_Constraint
  FD_HasValue :: FD_Term -> Int -> FD_Constraint
  FD_Eq   :: (ToExpr a, ToExpr b) => a -> b -> FD_Constraint
  FD_NEq   :: (ToExpr a, ToExpr b) => a -> b -> FD_Constraint
  FD_AllDiff :: [FD_Term] -> FD_Constraint
  FD_Dom     :: FD_Term -> (Int,Int) -> FD_Constraint


addFD (FD_Diff (FD_Var v1) (FD_Var v2)) = different v1 v2
addFD (FD_Same (FD_Var v1) (FD_Var v2)) = same      v1 v2
addFD (FD_Less (FD_Var v1) (FD_Var v2)) = v1 .<. v2
addFD (FD_LT (FD_Var v) i)              = do iv <- exprVar $ toExpr i
                                             v .<. iv
addFD (FD_GT (FD_Var v) i)              = do iv <- exprVar $ toExpr i
                                             iv .<. v
addFD (FD_HasValue (FD_Var v1) i)       = hasValue v1  i
addFD (FD_Eq e1 e2)                     = e1 .==. e2
addFD (FD_NEq e1 e2)                    = e1 ./=. e2 
-- addFD (FD_AllDiff vs)                   = allDifferent (map un_fd vs)
addFD (FD_Dom v (l,u))                  = v `in_range` (l-1,u+1)



data FD_Term where
  FD_Var :: FDVar -> FD_Term
  deriving Show

un_fd (FD_Var v) = v

newtype FDVar = FDVar { unFDVar :: Int } deriving (Ord, Eq, Show)


newtype Expr = Expr { unExpr :: FD (FDVar) }
class ToExpr a where
  toExpr :: a -> Expr
instance ToExpr FD_Term where
  toExpr (FD_Var v) = toExpr v
instance ToExpr FDVar where
  toExpr = Expr . return
instance ToExpr Expr where
  toExpr = id
instance Integral i => ToExpr i where
  toExpr n = Expr $ newVar n

exprVar :: ToExpr a => a -> FD FDVar
exprVar = unExpr . toExpr


newtype FD a = FD { unFD :: StateT FDState Maybe a }
    deriving (Functor, Applicative, Monad, MonadState FDState) -- MonadPlus)

data FDState = FDState
     { varSupply :: VarSupply, varMap :: VarMap, objective :: FDVar }
     deriving Show

type VarSupply = FDVar
type VarMap = Map FDVar VarInfo

data VarInfo = VarInfo
     { delayedConstraints :: FD Bool, domain :: Domain }

instance Show VarInfo where
  show x = show $ domain x

data Domain
    = Set IntSet
    | Range Int Int
    deriving Show


class ToDomain a where
    toDomain :: a -> Domain

instance ToDomain Domain where
    toDomain = id

instance ToDomain IntSet where
    toDomain = Set

instance Integral a => ToDomain [a] where
    toDomain = toDomain . IntSet.fromList . map fromIntegral

instance (Integral a, Integral b) => ToDomain (a, b) where
    toDomain (a, b) = Range (fromIntegral a) (fromIntegral b)

instance ToDomain () where
    toDomain () = Range minBound maxBound



newVar :: ToDomain a => a -> FD FDVar
newVar d = do
    s <- get
    let v = varSupply s
    put $ s { varSupply = FDVar (unFDVar v + 1) }
    modify $ \s ->
        let vm = varMap s
            vi = VarInfo {
                delayedConstraints = return True,
                domain = toDomain d}
        in
        s { varMap = Map.insert v vi vm }
    return v

newVars :: ToDomain a => Int -> a -> FD [FDVar]
newVars n d = replicateM n (newVar d)



-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: FD a -> a
runFD fd = fromJust $ evalStateT (unFD fd') initState
           where fd' = fd -- fd' = newVar () >> fd

initState :: FDState
initState = FDState { varSupply = FDVar 0, varMap = Map.empty, objective = FDVar 0 }


--------- Constraint Lib

-- Constrain two variables to have different values.
different :: FDVar  -> FDVar  -> FD Bool
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    if not (isSingleton xv) || not (isSingleton yv) || xv /= yv
       then whenwhen (isSingleton xv && xv `isSubsetOf` yv)
                     (isSingleton yv && yv `isSubsetOf` xv)
                     (update y (yv `difference` xv))
                     (update x (xv `difference` yv))
       else return False


-- Constrain two variables to have the same value.
same :: FDVar -> FDVar -> FD Bool
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = xv `intersection` yv
    if not $ null i
       then whenwhen (i /= xv)  (i /= yv) (update x i) (update y i)
       else return False


-- Constrain a variable to a particular value.
hasValue :: FDVar -> Int -> FD Bool
var `hasValue` val = do
    vals <- lookup var
    if val `member` vals
       then do let i = singleton val
               if (i /= vals) 
                  then update var i
                  else return True
       else return False


in_range :: FD_Term -> (Int,Int) -> FD Bool
in_range x (l,u) =
  do l #< x
     x #< u


class To_FD_Term a where
  to_fd_term :: a -> FD FD_Term

instance To_FD_Term FD_Term where
  to_fd_term = return . id

instance To_FD_Term Int where
  to_fd_term i =  newVar i >>= return . FD_Var

instance To_FD_Term Expr  where
  to_fd_term e = unExpr e >>= return . FD_Var



(#<) :: (To_FD_Term a, To_FD_Term b) => a -> b -> FD Bool
x #< y =
  do xt <- to_fd_term x
     yt <- to_fd_term y
     addFD (FD_Less xt yt)

-- Constrain one variable to have a value less than the value of another
-- variable.
infix 4 .<.
(.<.) :: FDVar -> FDVar -> FD Bool
(.<.) = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xv' = filterLessThan (findMax yv) xv
    let yv' = filterGreaterThan (findMin xv) yv
    if  not $ null xv'
        then if not $ null yv'
                then whenwhen (xv /= xv') (yv /= yv') (update x xv') (update y yv')
	        else return False
        else return False


infix 4 .==.
(.==.) :: (ToExpr a, ToExpr b) => a -> b -> FD Bool
xexpr .==. yexpr = do
    x <- exprVar xexpr
    y <- exprVar yexpr
    x `same` y


infix 4 ./=.
(./=.) :: (ToExpr a, ToExpr b) => a -> b -> FD Bool
xexpr ./=. yexpr = do
    x <- exprVar xexpr
    y <- exprVar yexpr
    x `different` y



--------------------Constraint Operators

whenwhen c1 c2 a1 a2  =
  if c1
     then do b1 <- a1
             if b1 
                then if c2
                        then a2
                        else return True
                else return False 
     else if c2
             then a2
             else return True



--------------------Constraint Adapter

 
-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint = FDVar -> FDVar -> FD Bool
addBinaryConstraint :: BinaryConstraint -> BinaryConstraint 
addBinaryConstraint f x y = do
    let constraint  = f x y
    b <- constraint 
    when b $ (do addConstraint x constraint
                 addConstraint y constraint)
    return b


-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar -> FD Bool -> FD ()
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap =
        Map.insert x (vi { delayedConstraints = do b <- cs 
                                                   if b then constraint
                                                        else return False}) vm }

--------------------Domain Adapter

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar -> Domain -> FD Bool
update x i = do
    trace (show x ++ " <- " ++ show i)  (return ())
    s <- get
    let vm = varMap s
    let vi = vm ! x
    trace ("where old domain = " ++ show (domain vi)) (return ())
    put $ s { varMap = Map.insert x (vi { domain = i}) vm }
    delayedConstraints vi


-- Lookup the current domain of a variable.
lookup :: FDVar -> FD Domain
lookup x = do
    s <- get
    return . domain $ varMap s ! x


-------------------Domain Lib

isSubsetOf :: Domain -> Domain -> Bool
isSubsetOf (Set xs) (Set ys) = xs `IntSet.isSubsetOf` ys
isSubsetOf (Range xl xh) (Range yl yh) = xl >= yl && xh <= yh
isSubsetOf (Set xs) yd@(Range yl yh) =
    isSubsetOf (Range xl xh) yd where
        xl = IntSet.findMin xs
        xh = IntSet.findMax xs
isSubsetOf (Range xl xh) (Set ys) =
    all (`IntSet.member` ys) [xl..xh]

difference :: Domain -> Domain -> Domain
difference (Set xs) (Set ys) = Set (xs `IntSet.difference` ys)
difference xd@(Range xl xh) (Range yl yh)
    | yl > xh || yh < xl = xd
    | otherwise = Set $ IntSet.fromList [x | x <- [xl..xh], x < yl || x > yh]
difference (Set xs) (Range yl yh) =
    Set $ IntSet.filter (\x -> x < yl || x > yh) xs
difference (Range xl xh) (Set ys)
    | IntSet.findMin ys > xh || IntSet.findMax ys < xl = Range xl xh
    | otherwise = Set $
        IntSet.fromList [x | x <- [xl..xh], not (x `IntSet.member` ys)]


intersection :: Domain -> Domain -> Domain
intersection (Set xs) (Set ys) = Set (xs `IntSet.intersection` ys)
intersection (Range xl xh) (Range yl yh) = Range (max xl yl) (min xh yh)
intersection (Set xs) (Range yl yh) =
    Set $ IntSet.filter (\x -> x >= yl && x <= yh) xs
intersection x y = intersection y x


isSingleton :: Domain -> Bool
isSingleton (Set xs) = case IntSet.elems xs of
    [x] -> True
    _   -> False
isSingleton (Range xl xh) = xl == xh

singleton :: Int -> Domain
singleton x = Set (IntSet.singleton x)

null :: Domain -> Bool
null (Set xs) = IntSet.null xs
null (Range xl xh) = xl > xh


findMax :: Domain -> Int
findMax (Set xs) = IntSet.findMax xs
findMax (Range xl xh) = xh

findMin :: Domain -> Int
findMin (Set xs) = IntSet.findMin xs
findMin (Range xl xh) = xl


filterLessThan :: Int -> Domain -> Domain
filterLessThan n (Set xs) = Set $ IntSet.filter (< n) xs
filterLessThan n (Range xl xh) = Range xl (min (n-1) xh)

filterGreaterThan :: Int -> Domain -> Domain
filterGreaterThan n (Set xs) = Set $ IntSet.filter (> n) xs
filterGreaterThan n (Range xl xh) = Range (max (n+1) xl) xh

member :: Int -> Domain -> Bool
member n (Set xs) = n `IntSet.member` xs
member n (Range xl xh) = n >= xl && n <= xh