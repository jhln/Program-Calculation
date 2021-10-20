{-# LANGUAGE FlexibleContexts #-}

module AbstarctSolver3 where


import qualified Data.Map as Map
import qualified Data.Set as Set


--- Model
--- Int => Varname, 
--- Maybe Bool => True | False | yet undefined for PosLitLit, NegLitLit
type Store = Map.Map Int (Maybe Bool, Maybe Bool)

--- for partial Building and Backtracking
--- Int => Varname, Bool => DecisionLit
type State = [(Int,Bool)]
emptyState :: State
emptyState = []


-------------------------
--- Syntax Literals

data Lit 
  = PosLit Int
  | NegLit Int
  deriving (Show, Eq)

instance Ord Lit where
  (PosLit i) <= (PosLit i') = i <= i'
  (NegLit i) <= (NegLit i') = i <= i'
  (NegLit i) <= (PosLit i')
    | i <= i' = True
    | otherwise = False
  (PosLit i) <= (NegLit i')
    | i < i' = True
    | otherwise = False


-------------------------
--- Literal Denotation

litId :: Lit -> Int
litId (PosLit i) = i
litId (NegLit i) = i

dualLit :: Lit -> Lit
dualLit (PosLit i) = NegLit i
dualLit (NegLit i) = PosLit i

denoteLit :: Lit -> Bool -> (Maybe Bool, Maybe Bool)
denoteLit (PosLit i) True = (Just True, Just False)
denoteLit (NegLit i) True = (Just False, Just True)
denoteLit (PosLit i) False = denoteLit (NegLit i) True
denoteLit (NegLit i) False = denoteLit (PosLit i) True


-------------------------
--- Store/Model Acess


storeLit :: Lit -> Store -> Store
storeLit lit store = 
  Map.update (const $ Just $ denoteLit lit True) (litId lit) store

storeDualLit :: Int -> Store -> Store
storeDualLit i store =
  Map.update (const $ Just dualValues) i store
  where
    dualValues = (\ (x, y) -> ( y, x)) $ store Map.! i

resetStoredId :: Int -> Store -> Store
resetStoredId = 
  Map.update (const $ Just (Nothing, Nothing))

evalLit :: Lit -> Store -> Maybe Bool
evalLit (PosLit i) store = fst $ store Map.! i
evalLit (NegLit i) store = snd $ store Map.! i


-------------------------
--- ContainerTypes

type Clause = [Lit]
type Formula = [Clause] 

allLitOfFormula :: (Ord Lit) => Formula -> Set.Set Lit
allLitOfFormula f = Set.fromList $ concat f

evalClause :: Clause -> Store -> Maybe Bool
evalClause clause store
  | any (== Just True) evaledLits = Just True
  | any (== Nothing) evaledLits = Nothing
  | otherwise = Just False
  where
    evaledLits :: [Maybe Bool]
    evaledLits = flip evalLit store <$> clause


cl1 = [NegLit 1, PosLit 2, NegLit 3]
cl2 = [NegLit 4, NegLit 5, PosLit 3, PosLit 2]
cl3 = [PosLit 6, NegLit 7, NegLit 2, NegLit 5]

fo1 = [cl1,cl2,cl3]

testAllLitOfFormula = allLitOfFormula fo1





pureLitOfFormula :: Formula -> (Set.Set Lit, Set.Set Lit)
pureLitOfFormula f = 
  Set.foldr check (Set.empty, Set.empty) $ allLitOfFormula f
  where
    check :: Lit -> (Set.Set Lit, Set.Set Lit) -> (Set.Set Lit,Set.Set Lit)
    check l (pure, notPure)
      | Set.member (dualLit l) pure = 
        ( Set.delete (dualLit l) pure
        , Set.insert l $ Set.insert (dualLit l) notPure)
      | Set.member l notPure = (pure, notPure)
      | otherwise = (Set.insert l pure, notPure)

testPureLitOfFormula = pureLitOfFormula fo1


buildStore :: Formula -> Store
buildStore f = 
  Map.fromList $ 
  do
    lit <- Set.elems allLits
    case lit of
      (PosLit i) -> return (i, (Nothing,Nothing))
      (NegLit i) -> return (i, (Nothing,Nothing))
  where
    allLits :: Set.Set Lit
    allLits = allLitOfFormula f



-------------------------
--- PureLit Formula

pureLit :: Formula -> Store -> State -> (Store, State)
pureLit f store state = (newStore, newState)
  where
    allPures :: Set.Set Lit
    allPures = fst $ pureLitOfFormula f
    newState = zip (litId <$> Set.elems allPures) (repeat False) ++ state
    {-
    updateStore :: Lit -> Store -> Store
    updateStore lit oldStore = Map.update (const $ Just $ denoteLit lit True) (litId lit) oldStore
    --newStore = Set.foldr updateStore store allPures
    -}
    newStore = Set.foldr storeLit store allPures

testPureLit :: Formula -> (Store, State)
testPureLit f = pureLit f (buildStore f) emptyState


-------------------------


unitOfClause :: Clause -> Store -> Maybe Lit
unitOfClause clause store = check Nothing clause 
  where
    check :: Maybe Lit -> Clause -> Maybe Lit
    check now [] = now
    check Nothing (lit:rest) =
      case (evalLit lit store) of
        Nothing -> check (Just lit) rest
        Just True -> Nothing
        Just False -> check Nothing rest
    check (Just unit) (lit:rest) =
      case (evalLit lit store) of
        Nothing -> Nothing
        Just True -> Nothing
        Just False -> check (Just unit) rest


cl4 = [NegLit 1, PosLit 2, NegLit 3]
cl5 = [NegLit 1, PosLit 2, PosLit 3]
cl6 = [PosLit 1, NegLit 2, NegLit 3]
fo2 = [cl4,cl5]

fo2Store = fst $  testPureLit fo2
testUnit1 = unitOfClause cl6 fo2Store

cl7 = [PosLit 1, PosLit 3]
testUnit2 =  unitOfClause cl7 fo2Store 
cl8 = [NegLit 2, NegLit 3]
testUnit3 =  unitOfClause cl8 fo2Store

unitOfFormula :: Formula -> Store -> Maybe Lit
unitOfFormula formula store = check Nothing formula
  where
    check :: Maybe Lit -> Formula -> Maybe Lit
    check now [] = now
    check Nothing (clause:rest) =
      case (unitOfClause clause store) of
        Nothing -> check Nothing rest
        Just lit -> Just lit

testUnitOfFormula1 = unitOfFormula [cl6] fo2Store
testUnitOfFormula2 = unitOfFormula [cl4,cl5,cl6] fo2Store 


-------------------------
--- UnitProp-Rule

unitProp :: Formula -> Store -> State -> (Store, State)
unitProp f store state = 
  case unit of
    Nothing -> (store, state)
    Just lit -> 
      let 
        newState = (litId lit, False) : state 
        newStore = storeLit lit store
      in
        (newStore, newState)
  where
    unit = unitOfFormula f store

testUnitProp1 = unitProp [cl4,cl5,cl6] fo2Store []


-------------------------
--- Decide


decide :: State -> Store -> Maybe (Store, State)
decide state store = 
  case undefined of
    [] -> Nothing
    _  -> Just (newStore, newState)
      where
        decidedId = fst $ fst undefined
        newState :: State
        newState = (decidedId,True) : state
        newStore :: Store
        newStore = storeLit (PosLit decidedId) store
  where
    undefinedLits :: [(Int, (Maybe Bool, Maybe Bool))]
    undefinedLits = 
      filter 
        (\(i, value) -> value == (Nothing, Nothing)) 
        $ Map.assocs store









-------------------------




conflictClause :: Clause -> Store -> Maybe Clause
conflictClause clause store = 
  case evalClause clause store of
    Just False -> Just clause
    otherwise  -> Nothing

backtrackOption :: State -> Maybe Int
backtrackOption [] = Nothing
backtrackOption ((i,True):rest) = Just i
backtrackOption ((_,False):rest) = backtrackOption rest

backtrackedState :: State -> (State , State, Maybe Int)
backtrackedState now = trace now ([], [], Nothing)
  where
    trace [] accu = accu
    trace ((i,True) : rest) (_, tested, _) = 
      (reverse $ tested, rest, Just i)
    trace ((i,False) : rest) (_, tested, _) =
      trace rest ([] , (i,False) : tested, Nothing)



-------------------------
--- FailState

failState :: Formula -> Store -> State -> Maybe (Store, State)
failState f store state =
  case conflict of
    Nothing -> Just (store, state)
    Just _  -> Nothing
  where
    conflict = check Nothing f
    check :: Maybe Clause -> Formula -> Maybe Clause
    check now [] = now
    check Nothing (clause:rest) =
      case (conflictClause clause store) of
        Just clause -> Just clause
        Nothing     -> check Nothing rest


-------------------------
--- Backprop-Rule

backtrack :: Formula -> Store -> State -> Maybe (Store, State)
backtrack f store state =      
  case (backtrackedState state) of
    ([],_,_)  -> Nothing
    (discards , staysInState, Just i) -> 
      Just (newStore, newState)
        where
          newStore = storeDualLit i $ foldr discardStore store discards
          newState = (i, False):staysInState
          discardStore :: (Int,Bool) -> Store -> Store
          discardStore (i,_) = resetStoredId i

