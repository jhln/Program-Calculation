module ReactiveUserInterfaceToolkit where


type Time = Double
type Signal a = Time -> a
type ST a b = Signal a -> Signal b

class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b,d) (c,d)



type EventSource α = Signal (Maybe α)

type GUIInput = (Maybe Kbd, Maybe Mouse)
data Kbd = K { keyDown :: [Char] }
data Mouse 
  = M { mpos :: Point,
      lbDown :: Bool,
      rbDown :: Bool }

type Point = (Double, Double)