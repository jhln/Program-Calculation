module PropLogicModelBuilder where

import Data.List (union)
import System.Random (split, randomR)
import System.Random.SplitMix (SMGen)
import Control.Monad (ap, liftM2)

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


-------- Generators


class Arbitrary a where
  arbitraty :: Gen a

newtype Gen a = MkGen {
  unGen :: SMGen -> Int -> a
}

instance Functor Gen where
  fmap f (MkGen h) =
    MkGen (\r n -> f (h r n))

instance Applicative Gen where
  pure x =
    MkGen (\_ _ -> x)
  (<*>) = ap

instance Monad Gen where
  return = pure

  MkGen m >>= k =
    MkGen (\r n ->
      case split r of
        (r1, r2) ->
          let MkGen m' = k (m r1 n)
          in m' r2 n
    )

  (>>) = (*>)

choose :: (Int, Int) -> Gen Int
--choose :: Random a => (a,a) -> Gen a
choose rng = MkGen (\r _ -> let (x,_) = randomR rng r in x)

instance Arbitrary Int where
  arbitraty = choose (-20,20)

instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitraty = liftM2 (,) arbitraty arbitraty


-------- Generators for User-Defined Types