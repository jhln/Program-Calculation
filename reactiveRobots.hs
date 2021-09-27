module ReactiveRobots where

type Time = Double
type Behavior a = Time -> a
type Event a = [(Time,a)]

