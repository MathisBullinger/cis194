-- exercise 1

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []

-- exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (zipWith (*) (reverse xs) (cycle [1, 2]))

-- exercise 3

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

-- exercise 4

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

-- exercise 5

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi n a b c = [(a, b)]

hanoi 0 a b c = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a
