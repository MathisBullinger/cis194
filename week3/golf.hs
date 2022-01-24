module Golf where

skip :: [a] -> [[a]]
skip x = map (nth x) [1..length x]

nth :: [a] -> Int -> [a]
nth list n = map (\(e, _) -> e) (filter (\(_, i) -> i `mod` n == 0) (zip list [1..]))
