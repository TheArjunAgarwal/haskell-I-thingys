import Data.List (sort)

-- Function to convert a list of strings into two lists of integers
makeList :: [String] -> ([Int], [Int])
makeList [] = ([], [])
makeList (x:y:xs) =
  let (l1, l2) = makeList xs
  in ((read x : l1), (read y : l2))
makeList _ = error "Invalid input format. Each row must contain two numbers."

-- Function to compute the sum of absolute differences
solve1 :: [Int] -> [Int] -> Int
solve1 [] [] = 0
solve1 (x:xs) (y:ys) = abs (x - y) + solve1 xs ys
solve1 _ _ = error "Lists must have the same length."

-- Main function to read input and produce output
-- main :: IO ()
-- main = do
  -- input <- getContents
  -- let (l1, l2) = makeList (words input)
  -- print $ solve1 (sort l1) (sort l2)

solve2 :: [Int] -> [Int] -> Int
solve2 [] _ = 0
solve2 _ [] = 0
solve2 (x:xs) (ys) = x * (count x ys) + solve2 xs ys where
  count r ls = sum [1 | l <- ls, l == r]

main = do
  input <- getContents
  let (l1, l2) = makeList (words input)
  print $ solve2 (sort l1) (sort l2)
