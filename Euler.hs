f :: Floating a => a -> a -> a
f x y = y/x - 4/(x*x)

g :: Floating a => a -> a -> a
g x y = x + y

eiler :: (Floating a, Enum a, Integral b) => (a -> a -> a) -> a -> a -> b -> a -> [a]
eiler f a b n y0 = drop 2 $ yk a b n y0
	where 
		yk a b n y0 = 0 : y0 : 
			zipWith 
				(\y x -> y + h*(f x y)) 
					(tail $ yk a b n y0) 
					([a + k*h | k<-[0 .. fromIntegral n]])

		h = (b-a)/ fromIntegral n		


main = do
	let
		a = 1
		b = 3
		n = 10
		y0 = 2

	print $ eiler f a b n y0
	print $ eiler g 0 0.4 4 1