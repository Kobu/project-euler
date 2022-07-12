fibSeq :: Int -> Int
fibSeq digs = fib 1 1
  where
    fib x y = if x > 10^(digs - 1) then 1 else 1 + fib y (x+y)
