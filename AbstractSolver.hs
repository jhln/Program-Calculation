module AbstractSolver where

data Var = Var 
    { name :: Int
    , value :: Maybe Bool}
    deriving (Eq)

instance Show Var where
  show (Var 
    { name = n
    , value = v}) =
      show n ++ " " ++ further
      where
        further =
          case v of
            (Just value)  -> show value
            otherwise     -> show "nothing"

var :: Int -> Maybe Bool -> Var
var i val = Var {name = i, value = val}

data Status = Undecided | Decided deriving (Eq, Show)

data Clause = Disj 
    { posVars :: [Var]
    , negVars :: [Var]}
    deriving (Eq)

instance Show Clause where
  show (Disj {posVars = ps, negVars = ns}) = 
    "Disj: " ++ " pos " ++ show ps ++ "  neg " ++ show ns


disj :: [Var] -> [Var] -> Clause
disj p n = Disj {posVars = p, negVars = n}

newtype Formula = Formula [Clause] deriving (Eq)

instance Show Formula where
  show (Formula clauses) = do 
    clause <- clauses
    (show clause) ++ "\n"


data Model 
    = Model { falses :: [Var]
            , trues :: [Var]
            , undecided :: [Var]
            , decisionLit :: [Var]}
    deriving (Eq, Show)

example = Formula [
  disj [] [var 1 Nothing, var 2 Nothing],
  disj [var 2 Nothing, var 3 Nothing] [],
  disj [var 4 Nothing] [var 1 Nothing, var 3 Nothing],
  disj [var 2 Nothing] [var 4 Nothing, var 3 Nothing],
  disj [var 1 Nothing, var 4 Nothing] []]

