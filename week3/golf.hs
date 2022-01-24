module Golf where

skip :: [a] -> [[a]]
skip x = [[fst e | e<-zip x [1..], mod (snd e) n == 0] | n<-[1..length x]] 

localMaxima :: [Integer] -> [Integer]
localMaxima x = [b | a:b:c:[] <- chunk x 3, a < b && b > c] 

chunk :: [a] -> Int -> [[a]]
chunk x n
    | length x < n = []
    | otherwise    = take n x : chunk (tail x) n 
