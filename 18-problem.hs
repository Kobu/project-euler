data Tree = Node Int Tree Tree | Nil deriving Show

split :: String -> [String]
split [] = [""]
split (c : cs)
  | c == ' ' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs

buildTree :: [[Int]] -> Tree
buildTree rows = buildTreeIter (reverse rows) (replicate (length rows + 1) Nil)
    where buildTreeIter rows acc = if null rows
                                   then head acc
                                   else buildTreeIter (tail rows) (joinRows (head rows) acc)
          joinRows parents children = zipWith3 Node parents children (drop 1 children)

maxPath :: Tree -> Int
maxPath tree = aux 0 tree
  where
    aux acc Nil = acc
    aux acc (Node val l r) = max (aux (acc + val) r) (aux (acc + val) l)

parse :: [String] -> [Int]
parse = map (read::String -> Int)

main :: IO ()
main = do
  file <- (readFile "./18-tree")
  let parsed = map (parse.words) (lines file)
  let tree = buildTree parsed
  print (maxPath tree)
