powerDigitSum :: Int -> Integer
powerDigitSum x = sumDigits $ 2 ^ x
  where
    sumDigits 0 = 0
    sumDigits n = n `mod` 10 + sumDigits (n `div` 10)
