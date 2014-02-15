module EulerSys (eulerSys) where

import Data.List

f :: Double -> [Double] -> [Double]
f x ys = [z, 2*x*z/(x*x+1)]
	where z = ys!!1

exactSol :: (Floating a) => a -> a
exactSol x = -x**3 + 3*x + 1


eulerSys :: (Floating a, Enum a, Integral b) => (a -> [a] -> [a]) -> a -> a -> b -> [a] -> [(a, [a])]
eulerSys f a b n y0 = take (1 + fromIntegral n) $ yk f a b n y0
	where 
		yk f a b n y0 = (a, y0) : map (\(x,y)->(x + h, newy x y)) (yk f a b n y0)

		h = (b-a) / fromIntegral n
		newy x y = zipWith (+) y $ map (*h) (f x y)


main = do
	print $ eulerSys f 0 1 5 [1,3]
