module RungeKutta where

f :: Floating a => a -> a -> a
f x y = sin(x) - cos(y)

runge_kutta4 :: (Floating a, Enum a, Integral b) => (a -> a -> a) -> a -> a -> b -> a -> [(a,a)]
runge_kutta4 f a b n y0 = zip xk (drop 1 $ yk f a b n y0)
	where 
		yk f a b n y0 = 0 : y0 : 
			zipWith 
				(\y x -> y + 1.0/6 * (k1 y x + 2 * (k2 y x) + 2 * (k3 y x) + k4 y x)) 
					(tail $ yk f a b n y0) 
					xk

		k1 y x = h * (f x y)
		k2 y x = h * (f (x + 0.5 * h) (y + 0.5 * (k1 y x)))
		k3 y x = h * (f (x + 0.5 * h) (y + 0.5 * (k2 y x)))
		k4 y x = h * (f (x + h) (y + k3 y x))

		h = (b-a) / fromIntegral n
		xk = [ a + k * h | k <- [0 .. fromIntegral n] ]