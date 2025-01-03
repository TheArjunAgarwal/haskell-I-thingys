sumod :: Int -> Int
sumod 0 = 0
sumod k = (k `mod` 10) + sumod (k `div` 10)

divsum :: Int -> Int
divsum n = sum [sumod x | x <- [1..n], n `mod` x == 0 ]

dsl :: Int -> [Int]
dsl 15 = [15]
dsl x = x: dsl (divsum x)

sum1 x = foldr (+) 0 x