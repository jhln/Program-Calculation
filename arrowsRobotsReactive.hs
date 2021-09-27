module ArrowsRobotsReactive where



type Time = Double
type Signal a = Time -> a
type SF a b = Signal a -> Signal b
{-
x = (1/2) * integral ((vr + vl) * cos theta)
y = (1/2) * integral ((vr + vl) * sin theta)
theta = (1/l) * integral (vr - vl)
-}


