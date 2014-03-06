module Adams (adams) where

import RungeKutta (runge_kutta4)
import Euler (euler)
import Data.List (zipWith4)

f :: Floating a => a -> a -> a
f x y = sin(x) - cos(y)


adams :: (Floating a, Enum a, Integral b) => (a -> a -> a) -> a -> a -> b -> a -> [(a,a)]
adams f a b n y0 = take (1+fromIntegral n) $ yk f a b n y0
	where
		yk f a b n y0 = (runge_kutta4 f a (a+h*3) 3 y0) ++
			zipWith4
				(\(x0,y0) (x1,y1) (x2,y2) (x3,y3) -> (x3+h, y3 + h/24*(55*(f x3 y3) - 59*(f x2 y2) + 37*(f x1 y1) - 9*(f x0 y0))))
					(yk f a b n y0) 
					(tail  $ yk f a b n y0) 
					(tail . tail $ yk f a b n y0) 
					(tail . tail . tail $ yk f a b n y0)

		h = (b-a) / fromIntegral n