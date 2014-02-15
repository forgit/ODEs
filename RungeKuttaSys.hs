module RungeKuttaSys (rk4Sys) where

import Data.List (zipWith4)

f :: Double -> [Double] -> [Double]
f x ys = [z, 2*x*z/(x*x+1)]
    where z = ys!!1

exactSol :: (Floating a) => a -> a
exactSol x = -x**3 + 3*x + 1


rk4Sys :: (Floating a, Enum a, Integral b) => (a -> [a] -> [a]) -> a -> a -> b -> [a] -> [(a, [a])]
rk4Sys f a b n y0 = take (1 + fromIntegral n) $ yk f a b n y0
    where
        yk f a b n y0 = (a,y0) : map (\(x,y)->(x + h, newy x y)) (yk f a b n y0)

        h = (b-a) / fromIntegral n
        newy x y = zipWith (+) y $ zipWith4 (\_k1 _k2 _k3 _k4 -> 1/6*(_k1 + 2*_k2 + 2*_k3 + _k4)) (k1 x y) (k2 x y) (k3 x y) (k4 x y)

        k1 x y = map (*h) (f x y)
        k2 x y = map (*h) (f (x+h/2) (zipWith (+) y $ map (*0.5) $ k1 x y))
        k3 x y = map (*h) (f (x+h/2) (zipWith (+) y $ map (*0.5) $ k2 x y))
        k4 x y = map (*h) (f (x+h) (zipWith (+) y $ k3 x y))


main = do
    print $ rk4Sys f 0 1 5 [1,3]