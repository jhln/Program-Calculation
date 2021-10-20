module AbstractSolver where

import Control.Monad (when)

data Atom = Atom 
    { name :: Int
    , value :: Maybe Bool}
    deriving (Eq)

instance Show Atom where
  show (Atom 
    { name = n
    , value = v}) =
      show n ++ " " ++ further
      where
        further =
          case v of
            (Just value)  -> show value
            otherwise     -> show "nothing"

atom :: Int -> Maybe Bool -> Atom
atom i val = Atom {name = i, value = val}

data Lit 
  = Pos Atom
  | Neg Atom
  deriving (Show, Eq)

evalLit :: Lit -> Maybe Bool
evalLit (Pos a) = value a
evalLit (Neg a) = 
  case value a of
    Nothing     -> Nothing
    Just True   -> Just False
    Just False  -> Just True

--data Status = Undecided | Decided deriving (Eq, Show)

--- unit propagate
unitPropCheck :: Clause -> Model -> Bool
unitPropCheck clause model = 
  (length (undecidedC clause) == 1)
  && (and 
      [ elem l (fst <$> (litsM model)) 
        && ((Just False) == evalLit l )
      | l <-decidedC clause])


data Clause = Disj 
    { litsC :: [Lit]
    , decidedC :: [Lit]
    , undecidedC :: [Lit]}
    deriving (Eq)

evalClause :: Clause -> Maybe Bool
evalClause clause  --foldr foldClause Nothing $ litsC clause
  | any (== Just True) evaledLits = Just True
  | any (== Nothing) evaledLits = Nothing
  | otherwise = Just False
  where 
    evaledLits = evalLit <$> litsC clause


instance Show Clause where
  show (Disj {litsC = l, undecidedC = u}) = 
    "Disj: \n" 
    ++ " LitsC" ++ show l
    ++ " \n"
    ++ " UndecC " ++ show u

disj :: [Lit] -> Clause
disj ls = Disj {litsC = ls, decidedC = [], undecidedC = ls}

data Formula = Formula 
    { clauses :: [Clause]
    , litsF :: [Lit]
    , undecidedF :: [Lit]} 
    deriving (Eq)

instance Show Formula where
  show (Formula 
    { clauses = cs, litsF = ls, undecidedF = unds}) = 
    "Formula: \n" 
    ++ "Clauses: \n" ++ (cs >>= \c -> show c ++ "\n" )
    ++ "LitsF :\n" ++ show ls ++ "\n"
    ++ "UndecF :\n" ++ show unds

evalFormula :: Formula -> Maybe Bool
evalFormula f 
  | any (== Just False) evaledClauses = Just False
  | any (== Nothing) evaledClauses = Nothing
  | otherwise = Just True
  where 
    evaledClauses = evalClause <$> clauses f


--- PureLiteral
pureLiteralCheck :: Lit -> Formula -> Bool
pureLiteralCheck (Pos a) form = notElem (Neg a) $ litsF form
pureLiteralCheck (Neg a) form = notElem (Pos a) $ litsF form

--- Decide
decideCheck :: Formula -> Bool
decideCheck f 
  | null $ undecidedF F = False
  | otherwise = undefined

--- Fail

--- Backtrack


formula cs = Formula
  { clauses = cs
  , litsF = cs >>= litsC
  , undecidedF = cs >>= litsC}

data Model 
    = Model { litsM :: [(Lit,Bool)]
            --, undecided :: [Lit]
            --, decisionLits :: [Lit]
            }
    deriving (Eq)

instance Show Model where
  show (Model { litsM = ls
            --, undecided :: [Lit]
            --, decisionLits = dls
            }) = 
    "Model: " 
    ++ "decided " 
    -- ++ show ls
    ++ do 
        (lit,b) <- ls
        if b
          then " (DecLit)" ++ show lit
          else show lit
        -- when b return " (DecLit)"
        -- (++) $ show lit 
    ++ "\n"

model = Model { litsM = []
              --, decisionLits = []
              }

exampleClauses = [
  disj [Neg $ Atom 1 Nothing, Neg $ Atom 2 Nothing],
  disj [Pos $ Atom 2 Nothing, Pos $ Atom 3 Nothing],
  disj [Pos $ Atom 4 Nothing, Neg $ Atom 1 Nothing, Neg $ Atom 3 Nothing],
  disj [Pos $ Atom 2 Nothing, Neg $ Atom 4 Nothing, Neg $ Atom 3 Nothing],
  disj [Pos $ Atom 1 Nothing, Pos $ Atom 4 Nothing]]

example = formula exampleClauses

