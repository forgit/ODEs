f :: Floating a => a -> a -> a
f x y = sin(x) - cos(y)

eiler :: (Floating a, Enum a, Integral b) => (a -> a -> a) -> a -> a -> b -> a -> [(a,a)]
eiler f a b n y0 = zip xk (drop 1 $ yk a b n y0)
	where 
		yk a b n y0 = 0 : y0 : 
			zipWith 
				(\y x -> y + h * (f x y)) 
					(tail $ yk a b n y0) 
					xk

		h = (b-a) / fromIntegral n		
		xk = [ a + k * h | k <- [0 .. fromIntegral n] ]

main = print $ eiler f 0 1 5 1