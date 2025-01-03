solve1 :: [Int] -> Bool
solve1 [] = True
solve1 [a] = True
solve1 l@(x:y:ys)
    | x > y = dec l
    | x < y = inc l
    | x == y = False where
        inc :: [Int] -> Bool
        inc [] = True
        inc [a] = True
        inc l = all (\(x,y) -> if (y - x) < 4 && (y-x) > 0 then True else False ) (zip l (tail l))
        dec :: [Int] -> Bool
        dec [] = True
        dec [a] = True
        dec l = all (\(x,y) -> if (x - y) < 4 && (x-y) > 0 then True else False ) (zip l (tail l))

solve2 :: [Int] -> Bool
solve2 [] = True
solve2 [a] = True
solve2 l@(x:y:ys)
    | x > y = decs l
    | x < y = incs l
    | x == y = solve1 (y:ys) where
        pairs = zip l (tail l)
        incs :: [Int] -> Bool
        incs [] = True
        incs [a] = True
        incs l = length (filter (\(x, y) -> not (y - x > 0 && y - x < 4)) pairs) < 2
        decs :: [Int] -> Bool
        decs [] = True
        decs [a] = True
        decs l = length (filter (\(x, y) -> not (x - y > 0 && x - y < 4)) pairs) < 2
main = do
    i <- getContents
    let l = map ((map read).words) (lines i)
    print $ length (filter solve2 l)