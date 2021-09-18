module ProbabilisticFunctional where


-- https://github.com/VictorCMiraldo/mmm
-- eecs.oregonstate.edu/~erwig/pfp/



newtype Probability = P Float
newtype Dist a = D {unD :: [(a,Probability)]}

type Spread a = [a] -> Dist a

