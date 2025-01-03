lcs :: String -> String -> Int
lcs "" _ = 0
lcs _ "" = 0

lcs (x:xs) (y:ys)
    | x==y          =1 + lcs xs ys 
    | otherwise     =max (lcs (x:xs) ys) (lcs xs (y:ys))

lcsstr :: String -> String -> String
lcsstr _ "" = ""
lcsstr "" _ = ""
lcsstr (x:xs) (y:ys)
  | x==y                                                    = x: lcsstr xs ys
  | length (lcsstr (x:xs) ys) > length (lcsstr xs (y:ys))   = lcsstr (x:xs) ys
  | otherwise                                               = lcsstr xs (y:ys)


lcsn :: String -> String -> Int
lcsn xs ys = table !! (length xs) !! (length ys)
  where
    table = [[lcsDP i j | j <- [0..length ys]] | i <- [0..length xs]]
    lcsDP 0 _ = 0
    lcsDP _ 0 = 0
    lcsDP i j
      | xs !! (i-1) == ys !! (j-1)        = 1 + table !! (i-1) !! (j-1)
      | otherwise                         = max (table !! (i-1) !! j) (table !! i !! (j-1))

lcsstrn :: String -> String -> String
lcsstrn xs ys = reverse $ table !! (length xs) !! (length ys)
  where
    table = [[lcsstrDP i j | j <- [0..length ys]] | i <- [0..length xs]]
    lcsstrDP 0 _ = ""
    lcsstrDP _ 0 = ""
    lcsstrDP i j
      | xs !! (i-1) == ys !! (j-1)                                      = xs !! (i-1) : table !! (i-1) !! (j-1)
      | length (table !! (i-1) !! j) > length (table !! i !! (j-1))     = table !! (i-1) !! j
      | otherwise                                                       = table !! i !! (j-1)




ps :: [Int]
ps = [0,   1,   5,   8,   9,  10,  17,  17,  20,  24, 30,  32,  35,  39,  43,  43,  45,  49,  50,  54,  57,  60,  65,  68,  70,  74,  80,  81,  84,  85,  87,  91,  95,  99, 101, 104, 107, 112, 115, 116, 119]

cutLog :: [Int] -> Int -> Int
cutLog prices n = (map (\x -> cutLog' prices x) [0..]) !! n where
  cutLog' _ 0 = 0
  cutLog' prices l = maximum $ zipWith ( \ i price -> price + cutLog prices (l-i) ) [1..l] (tail prices)