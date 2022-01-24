module Golf where

import Data.List

skip :: [a] -> [[a]]
skip x = [[fst e | e<-zip x [1..], mod (snd e) n == 0] | n<-[1..length x]] 

localMaxima :: [Integer] -> [Integer]
localMaxima x = [b | a:b:c:[] <- [take 3 $ drop i x | i <- [0..length x - 3]], a < b && b > c] 

histogram :: [Integer] -> String
histogram x = intercalate "\n" (c (map fromIntegral x) ++ [l '=', ['0'..'9']])

c :: [Int] -> [String]
c x = foldl (\a b -> let (h,t) = splitAt b (a!!0) in
        if a!!0!!b == ' '
          then r b "*" (a!!0) : drop 1 a 
          else (r b "*" $ l ' ') : a
      ) [l ' '] x

r :: Int -> String -> String -> String
r i c s = let (h,t) = splitAt i s in h ++ c ++ drop 1 t

l :: Char -> String
l c = replicate 10 c
