module Golf where

skip :: [a] -> [[a]]
skip x = [[fst e | e<-zip x [1..], mod (snd e) n == 0] | n<-[1..length x]] 
