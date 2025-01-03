

(!!!) :: Eq a => [a] -> a -> Int
xs !!! x = go xs x 0 where
    go [] _ _ = Error 

insertAtAll :: a-> [a] -> [[a]]
insertAtAll x [] = [[x]]  -- if the list is empty, the only possible list is [x]
insertAtAll x lst = insertAtAllHelper lst x 0
  where
    insertAtAllHelper :: [a] -> a -> Int -> [[a]]
    insertAtAllHelper [] x _ = [[x]]  -- handle empty list, insert at the beginning
    insertAtAllHelper (y:ys) x i = (x:y:ys) : map (y:) (insertAtAllHelper ys x (i+1))

check :: [Int] -> Bool
check p =
    let n = length p
        indices = [(i, j) | i <- [1..(n-1)], j <- [1..(n-1)], i `mod` 2 == j `mod` 2]
        satisfiesCondition (i, j) =
            let pi = p !!! i
                pi1 = p !!! (i + 1)
                pj = p !!! j
                pj1 = p !!! (j + 1)
            in  (pi < pj && pj < pi1 && pi1 < pj1) ||
                (pj < pi1 && pi1 < pj1 && pj1 < pi) ||
                (pi1 < pj1 && pj1 < pi && pi < pj) ||
                (pj1 < pi && pi < pj && pj < pi1)
    in  not (any satisfiesCondition indices)


folds :: Int -> [[Int]]
folds 1 = [[1]]
folds 2 = [[1,2],[2,1]]
folds n = concatMap (filter check . insertAtAll n) (memo !! (n-2))

memo = [folds x | x <- [1..]]

main = print $ length $ folds 12