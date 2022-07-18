coins :: [Int] -> Int -> Int
coins values price
  | price == 0 = 1
  | price < 0 = 0
  | price > 0 && null values = 0
coins values price = coins values (price - head values) + coins (tail values) price
