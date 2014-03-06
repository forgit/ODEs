module Euler (euler) where

f :: Floating a => a -> a -> a
f x y = sin(x) - cos(y)

-- implementation with map
euler :: (Floating a, Enum a, Integral b) => (a -> a -> a) -> a -> a -> b -> a -> [(a,a)]
euler f a b n y0 = take (1+fromIntegral n) $ yk f a b n y0
	where 
		yk f a b n y0 = (a,y0) : map (\(x,y) -> (x+h, y + h * (f x y))) (yk f a b n y0)

		h = (b-a) / fromIntegral n

-- implementation with zipWith
_euler :: (Floating a, Enum a, Integral b) => (a -> a -> a) -> a -> a -> b -> a -> [(a,a)]
_euler f a b n y0 = zip xk (drop 1 $ yk a b n y0)
	where 
		yk a b n y0 = 0 : y0 : 
			zipWith 
				(\y x -> y + h * (f x y)) 
					(tail $ yk a b n y0) 
					xk

		h = (b-a) / fromIntegral n		
		xk = [ a + k * h | k <- [0 .. fromIntegral n] ]