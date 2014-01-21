import Adams (adams)
import RungeKutta (runge_kutta4)
import Euler (euler)

f :: Floating a => a -> a -> a
f x y = sin(x) - cos(y)

main = do
	print $ map snd $ adams f 0 1 10 1
	print $ map snd $ runge_kutta4 f 0 1 10 1
	print $ map snd $ euler f 0 1 10 1