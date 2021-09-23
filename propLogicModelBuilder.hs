module PropLogicModelBuilder where

import Data.List (union)

newtype Name 
  = Name String 
  deriving (Eq, Show)

data Form 
  = Var Name
  | Form :&: Form
  | Not Form
  deriving (Eq, Show)


names :: Form -> [Name]
name (Var v) = [v]
names (p :&: q) = names p `union` names q
names (Not p) = names p


data Valuation 
    = Val { falses :: [Name],
            trues :: [Name]}
    deriving (Eq, Show)


nothing :: Valuation
nothing = Val { falses = [], trues = []}


tableau :: Form -> Valuation -> [Valuation]
tableau (Var v) val
  | v `elem` trues val  = [val]
  | v `elem` falses val = []
  | otherwise           = [ val { trues = v: trues val}]
tableau (Not (Var v)) val
  | v `elem` trues val  = []
  | v `elem` falses val = [val]
  | otherwise           = [ val { falses = v: falses val}]
tableau (p :&: q) val   = [ val'' 
                          | val'  <- tableau p val, 
                            val'' <- tableau q val']
tableau (Not (p :&: q)) val   
    = tableau (Not p) val ++ tableau (Not q) val

models :: Form -> [Valuation]
models p = tableau p nothing


