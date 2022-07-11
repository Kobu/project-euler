prime :: Int -> Int
prime n = getPrime n 2
  where
    getPrime 0 x = x
    getPrime y x = if all id [x `mod` i /= 0 | i <- [2..x]] then getPrime (y-1) (x+2) else getPrime y (x+2)
