sumSquareDiff :: Int -> Int
sumSquareDiff x = (sum [1..x] ^ 2) - sum [ i ^ 2 | i <- [1..x]] 
