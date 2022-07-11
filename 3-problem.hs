largestPrimeFactor :: Int -> Int
largestPrimeFactor n = auxLPF 2 n
  where
    auxLPF p 1 = p
    auxLPF p n = if n `mod` p == 0 then auxLPF p (div n p) else auxLPF (p + 1 + fromEnum (odd p)) n
