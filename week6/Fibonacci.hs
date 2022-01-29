import Data.Char

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-2) + (fib (x-1))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 50 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : (streamToList t)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons h t) = Cons (f h) (streamMap f t)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f $ f s)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap largestP2Div $ streamFromSeed (+1) 1

largestP2Div :: Integer -> Integer
largestP2Div x = head $ filter (\e -> x `mod` 2^e == 0) $ reverse $ takeWhile ((x>=) . (2^)) [0..]

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x ys) zs = Cons x $ interleaveStreams zs ys

ruler' :: Stream Integer
ruler' = start 0
  where start x = interleaveStreams (streamRepeat x) (start (x+1))
