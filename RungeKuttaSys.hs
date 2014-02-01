-- https://sites.google.com/site/srihariramanathan/runge-kuttawithhaskell
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/


instance (Num a) => Num [a] where
    (+) = zipWith (+)
    negate = fmap negate
    fromInteger = undefined
    (*) = undefined
    abs = undefined
    signum = undefined

integrate_system :: (Functor f, Floating a, Num (f a)) =>
                    (f a -> f a) -> f a -> a -> [f a]
integrate_system system_derivative initial_state h =
    let next = runge_kutta4 system_derivative h
        runge_kutta4 f h y =
            let
                (.*) n = fmap (*n)
                shf = ((1/2).*)
                k0 = (f y) 
                k1 = h .* (y + shf k0)
                k2 = h .* (y + shf k1)
                k3 = h .* (y + k2)
            in
              y + (h/6) .* (k0 + 2 .* k1 + 2 .* k2 + k3)
    in
      iterate next initial_state

runge_kutta4 f h y = y + (h/6) .* (k0 + 2 .* k1 + 2 .* k2 + k3)
            where
                (.*) n = fmap (*n)
                shf = ((1/2).*)
                k0 = (f y) 
                k1 = h .* (y + shf k0)
                k2 = h .* (y + shf k1)
                k3 = h .* (y + k2)
                    

rossler a b c (x:y:z:_) = [-(y+z), x+a*y, b + x*z - c*z]
lorenz s r b (x:y:z:_) = [s * (y - z), r * x - y - x * z, x * y - b * z]

--expEq k = integrate_system (\(y:_) -> y) 

--f :: (Functor f, Floating a, Num (f a)) => f a -> f a
f y = y

g x y = sin(x) - cos(y)

main = do
    print $ 0
    --print $ integrate_system f [1,2] 0.1
    print $ runge_kutta4 f 0.1 [1,2,3]
    --print $ let (.*) n = fmap (*n) in 2.* (f [1])
    print $ f [1,2]
