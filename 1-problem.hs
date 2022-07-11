calculate :: Int -> Int
calculate x = sum (filter (\n -> n `mod` 5 == 0 || n `mod` 3 == 0) [1..x-1])
