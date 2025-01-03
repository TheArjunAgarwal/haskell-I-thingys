newtype Vector a = Vec [a] -- all vectors are 1 x n

vectorToLis :: Vector a -> [a]
vectorToLis (Vec xs) = xs

lisToVector :: [a] -> Vector a
lisToVector = Vec

(<+>) :: Num a => Vector a -> Vector a -> Vector a
(Vec xs) <+> (Vec ys) = Vec (zipWith (+) xs ys)

size :: Vector a -> Int
size (Vec xs) = length xs
 
newtype Matrix a = M [Vector a]

lisToMatrix :: [[a]] -> Matrix a
lisToMatrix xs = M (map lisToVector xs)

dimension :: Matrix r -> (Int, Int)
dimension (M xs) 
    | null xs   = (0,0)
    | otherwise = (length xs, length (vectorToLis (head xs)))  -- (r,c)

addM :: Num r => Matrix r -> Matrix r -> Matrix r
addM (M xs) (M ys)
  | dimension (M xs) == dimension (M ys) = m
  | otherwise = error "Bad dimensions in matrix addition"
  where
    m  = M (zipWith (<+>) xs ys)

identity :: Int -> Matrix Int
identity n = lisToMatrix (go 0)
  where
    go x 
      | x == n    = []
      | otherwise = (replicate x 0 ++ [1] ++
                      replicate (n-x-1) 1) : go (x+1)
