module ReactiveProgContinued where


import Data.Stream

type Time = Double
type Behavior a = Time -> a
type Event a = [(Time,a)]

type Signal a = Time -> a
type SP a b = Stream a -> Stream b

type DTime = Double
data SF a b = SF { sfTF :: DTime -> a -> (SF a b, b) }

class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b,d) (c,d)


instance Arrow SF where
  arr = undefined
  --(>>>) :: SF a b -> SF b c -> SF a c
  (SF {sfTF = tf1}) >>> (SF {sfTF = tf2}) =
    SF {sfTF = tf}
    where
      tf dt a = (sf1' >>> sf2', c)
        where
          (sf2', c) = tf2 dt b
          (sf1', b) = tf1 dt a
  first = undefined

constant :: b -> SF a b
constant b = SF {sfTF = \ _ _-> (constant b,b)}

identity :: SF a a
identity = SF {sfTF = \ _ a -> (identity,a)}

sfArr :: (a -> b) -> SF a b
sfArr f = SF {sfTF = \ _ a -> (sfArr f,f a)}

