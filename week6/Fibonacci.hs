fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-2) + (fib (x-1))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
