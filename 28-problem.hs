spiralSum :: Int -> Int
spiralSum dimension = calc ((dimension - 1) `div` 2)
  where
    calc 0 = 1
    calc x = 4 * (2 * x + 1) ^ 2 - 12 * x + calc (x - 1)
