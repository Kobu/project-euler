fibEvenSum :: Int -> Int
fibEvenSum limit = fibAux 1 2
  where
    fibAux a b = if a > limit then 0 else (a * fromEnum (even a)) + fibAux b (a+b)
