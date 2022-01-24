module Golf where

import Data.List

skip :: [a] -> [[a]]
skip x = [[fst e | e<-zip x [1..], mod (snd e) n == 0] | n<-[1..length x]] 

localMaxima :: [Integer] -> [Integer]
localMaxima x = [b | a:b:c:[] <- [take 3 $ drop i x | i <- [0..length x - 3]], a < b && b > c] 

histogram :: [Integer] -> String
histogram x = intercalate "\n" $
              transpose [
                reverse $ take 10 $ replicate (length $ filter (n==) x) '*' ++ repeat ' '
                | n <- [0..9]
              ]
              ++ ["==========", ['0'..'9']]
