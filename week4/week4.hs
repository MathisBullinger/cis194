-- exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (+(-2)) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = foldl (+) 0 . filter even . takeWhile (/=1) . iterate (\n -> if even n then div n 2 else 3*n+1)


-- exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (\el tree -> fst $ addNode tree el) Leaf

addNode :: (Tree a) -> a -> (Tree a, Integer)
addNode tree el = case tree of
  Leaf         -> (Node 0 Leaf el Leaf, 0)
  Node n l d r -> let (ln, lc) = addNode l el
                      (rn, rc) = addNode r el
                  in if rc < lc
                    then (Node (1+rc) l d rn, 1+rc)
                    else (Node (1+lc) ln d r, 1+lc)
