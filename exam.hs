import Data.List


inverse :: (Integer -> Integer) -> Integer -> Integer
inverse _ 0 = 0
inverse f n = search f 1 (n+1) n - 1 where
    search :: (Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
    search f lb ub b
        | lb == ub = lb  -- If lb and ub converge, we've found our answer
        | otherwise = 
            if f mid > b 
                then search f lb mid b 
                else search f (mid+1) ub b  
                    where mid = (ub + lb) `div` 2

maximums :: [Int] -> [Int]
maximums l = solve l [] where
    solve [] acc = reverse acc
    solve (x:xs) [] = solve xs [x]
    solve (x:xs) (y:ys) = if x > y then solve xs (x:y:ys) else solve xs (y:y:ys)

isZigZag :: [Int] -> Bool
isZigZag [] = True
isZigZag [a] = True
isZigZag [a,b] = a <= b
isZigZag (x:y:z:xs) = (x <= y && y >= z) && isZigZag (z:xs)

fcolletz :: Integer -> Integer
fcolletz n = genericIndex (map colletz [0..]) n
colletz :: Integer -> Integer
colletz 0 = 0
colletz 1 = 0
colletz n = if even n then 1+ fcolletz (n `div` 2) else 1 + fcolletz (3*n +1)