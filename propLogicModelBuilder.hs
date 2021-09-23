module PropLogicModelBuilder where

import Data.List (union)
import System.Random (split, Random, randomR, StdGen, newStdGen)
import System.Random.SplitMix (SMGen, newSMGen)
import Control.Monad (ap, liftM, liftM2)

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
  arbitrary :: Gen a

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

--choose :: (Int, Int) -> Gen Int
choose :: Random a => (a,a) -> Gen a
choose rng = MkGen (\r _ -> let (x,_) = randomR rng r in x)

instance Arbitrary Int where
  arbitrary = choose (-20,20)

instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = liftM2 (,) arbitrary arbitrary


-------- Generators for User-Defined Types

data Colour
  = Red
  | Blue
  | Green

instance Arbitrary Colour where
  arbitrary = oneof [return Red, return Blue, return Green]

oneof :: [Gen a] -> Gen a
oneof [] = error "QuickCheck.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

--instance Arbitrary a => Arbitrary [a] where
--  arbitraty = oneof [return [], liftM2 (:) arbitraty arbitraty]


instance Arbitrary a => Arbitrary [a] where
  arbitrary = frequency 
    [ (1, return [])
    , (4, liftM2 (:) arbitrary arbitrary)]

frequency :: [(Int, Gen a)] -> Gen a
frequency [] = error "QuickCheck.frequency used with empty list"
frequency xs
  | any (< 0) (map fst xs)  = error "QuickCheck.frequency: negative weight"
  | all (== 0) (map fst xs) = error "QuickCheck.frequency: all weights were zero"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)
  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "QuickCheck.pick used with empty list"



data Tree a 
  = Leaf a
  | Branch (Tree a) (Tree a)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency 
    [ (1, liftM Leaf arbitrary)
    , (2, liftM2 Branch arbitrary arbitrary)]

--newtype QCGen = QCGen StdGen
newtype QCGen = QCGen SMGen
--newQCGen = fmap QCGen newStdGen
newQCGen = fmap QCGen newSMGen


generate :: Gen a -> IO a
generate (MkGen g) =
  do r <- newSMGen -- newQCGen
     return (g r 30)

elements :: [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

test1 = generate $ elements [1,2,3]
test2 = generate $ choose ('a', 'z')
test3 = generate $ return 1

instance Arbitrary Bool where
  arbitrary = choose (False, True)
  --arbitrary = oneof [False, True]

data MyType = MyType {
    foo :: Int
  , bar :: Bool
  , baz :: Bool
  } deriving (Show)

test4 = generate $ MyType <$> arbitrary <*> arbitrary <*> arbitrary

myList :: Arbitrary a => Gen [a]
myList = oneof
  [ return []
  , (:) <$> arbitrary <*> myList
  ]


myList' :: Arbitrary a => Gen [a]
myList' = frequency
  [ (1, return [])
  , (4, (:) <$> arbitrary <*> myList')
  ]


instance Arbitrary Form where
  arbitrary = frequency
    [ (1, liftM Var arbitrary)
    , (2, liftM2 (:&:) arbitrary arbitrary)
    , (2, liftM Not arbitrary)]

{- 
data Form 
  = Var Name
  | Form :&: Form
  | Not Form
  deriving (Eq, Show)

data Tree a 
  = Leaf a
  | Branch (Tree a) (Tree a)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency 
    [ (1, liftM Leaf arbitrary)
    , (2, liftM2 Branch arbitrary arbitrary)]
-}

instance Arbitrary Name where
  -- arbitrary = oneof [return $ Name "a", return $ Name "z"]
  -- arbitrary = choose (Name "a", Name "z")
  arbitrary = Name <$> arbitrary

instance Arbitrary Char where
  arbitrary = choose ('a', 'z')


test6 = generate (arbitrary :: Gen Form)