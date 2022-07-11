largestProduct :: Int -> [Int] -> Int
largestProduct n x = largestProductAux (product (take n x)) (drop 1 x)
  where
    largestProductAux max [] = max
    largestProductAux max ns
      = if product (take n ns) > max then
          largestProductAux  (product (take n ns)) (drop 1 ns)
        else
          largestProductAux max (drop 1 ns)
main :: IO ()
main = do
  file <- readFile "number-8"
  let number = map (read.pure::Char -> Int) (init file)
  print (largestProduct 13 number)
