import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F

euler :: (Floating a, Enum a, Integral b) => (a -> a -> a) -> a -> a -> b -> a -> [(a,a)]
euler f a b n y0 = zip xk (drop 1 $ yk a b n y0)
	where 
		yk a b n y0 = 0 : y0 : 
			zipWith 
				(\y x -> y + h * (f x y)) 
					(tail $ yk a b n y0) 
					xk

		h = (b-a) / fromIntegral n		
		xk = [ a + k * h | k <- [0 .. fromIntegral n] ]

--g = \x -> fmap (+) $ return x

h = [(\x y->x+y-4),(\x _->x+2)]
q = [(\x->x+2),(\y->y-1)]


--f = [(\x _->sin x),(\_ y->sin y)]

yk f a b n y0 = (a,y0) : map (\(x,y) -> (x+h, y + h * (f x y))) (yk f a b n y0)
	where h = (b-a) / fromIntegral n

--yk' f@(ff:fs) a b n y0@(yy:ys) = (a,y0) : map (\(x,y) -> (x+h, map (id) y)) (yk' f a b n y0)

--yk' :: (Floating a, Enum a, Integral b) => [(a -> a -> a->a)] -> a -> a -> b -> [a] -> [(a,[a])]
yk' f a b n y0 = (a,y0) : map (\(x,y) -> (x+h, zipWith (+) (map (*h) (apply f (x:y))) y)) (yk' f a b n y0)
	where h = (b-a) / fromIntegral n

f :: [Double -> Double -> Double -> Double]
f = [(\x y1 y2 -> x+y1+y2),(\x y1 y2 -> x-y1-y2)]

apply fs xs = map (\f -> foldl1 f xs) fs

aa fs xs = fs <*> (head xs)

bb [] = []
bb xs = head xs <*> bb (tail xs)

--cc f xs = f <$> 

main = do
--	print $ take 11 $ yk (\ x y -> sin(x) - cos(y)) 0 1 10 1
--	print $ take 11 $ yk' [(\ x y -> sin(x) - cos(y))] 0 1 10 [1]
	--print $ take 11 $ yk' f 0 1 10 [1,1]

	--print $ apply h [1,2]
	--print $ apply f [1,2,3]
	print $ (\x y z-> x+y+z) <$> [2] <*> [3] <*> [4]
	print $ f <*> [1] <*> [3] <*> [3]
	print $ [(+)] <*> [1] <*> [2]
	--print $ F.foldrM (<*>) [[(+)],[1],[2]]
	print $ F.fold ([[1,2]])
	print $ F.toList $  [1,2,3]
	print $ 







