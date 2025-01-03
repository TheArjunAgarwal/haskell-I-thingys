import Data.Complex
import Data.Char (ord, chr)
import Data.Ratio

encode :: String -> [(Int, Char)]
encode [] = []
encode (x:xs) = (length (takeWhile (==x) (x:xs)), x) : encode (dropWhile (==x) xs)

isAP :: [Integer] -> Bool
isAP [] = True
isAP [_] = True
isAP [_, _] = True
isAP (x:y:ys) = all (== (y-x)) (zipWith (-) ys (y:ys))


targetSum :: Integer -> [Integer] -> Bool
targetSum _ [] = False
targetSum t (x:xs) = t `elem` map (+x) xs || targetSum t xs

isort [] = []
isort (x:xs) = isort (filter (x>=) xs) ++ [x] ++ isort (filter (x<) xs)

strchr :: [Char] -> [Int]
strchr = map (\x -> ord x - ord 'a' + 1)

chrstr :: [Int] -> [Char]
chrstr = map (\x -> chr (x + ord 'a'))

caesershift :: Int -> [Char] -> [Char]
caesershift n  = chrstr . map (\x -> (x + n) `mod` 26) . strchr

cwtb :: [Rational] -> [Rational]
cwtb n = concat [ [(numerator x + denominator x) % denominator x, numerator x % (numerator x + denominator x)] | x <- n]

cwtf :: Integer -> [Rational]
cwtf 1 = [1 % 1]
cwtf n = cwtb (cwtf (n - 1))

cwt :: [Rational]
cwt = concatMap cwtf [1..]

postfin :: (Eq a) => [a] -> a -> Integer
postfin [] _ = 1
postfin (x:xs) y = acc (x:xs) y 0 
  where acc [] _ a = a
        acc (x:xs) y a = if x == y then a else acc xs y (a + 1)

enrat :: Rational -> Integer
enrat n = postfin cwt (red n) + 1

red :: Rational -> Rational
red n = (a `div` gcd a b) % (b `div` gcd a b) 
  where 
    a = numerator n
    b = denominator n


repstr :: Rational -> [Int]
repstr x 
    | numerator x > denominator x  = 0:repstr ((numerator x - denominator x) % denominator x)
    | numerator x < denominator x  = 1:repstr (numerator x % (denominator x - numerator x))
    | otherwise = [1]

fenrat :: Rational -> Integer
fenrat n = foldl (\x  y -> 2*toInteger x + toInteger y) 0 (repstr (red n))

subseq :: [Char] -> [Char] -> Bool
subseq [] [] = True
subseq [] (y:ys) = True
subseq (x:xs) [] = False
subseq (x:xs) y = subseq xs (dropWhile (/= x) y)

elemIndex :: Char -> String -> Maybe Int
elemIndex x y = if not(x `elem` y) then Nothing else Just (accIndex x y 0) where accIndex x (y:ys) a = if x == y then fromInteger(a) else accIndex x ys (a+1)

fibAndCount :: Int -> (Int, Int)
fibAndCount 0 = (0,0)
fibAndCount 1 = (1,0)
fibAndCount n = ((fst (fibAndCount (n-1)) + fst (fibAndCount (n-2))), (snd (fibAndCount (n-1)) + snd (fibAndCount (n-2)) + 1))

value :: [Int] -> Int
value [1] = 0
value [2] = 1
value x = f x 0 where f x a = if (x == []) then a else f (tail x) (2*a + (head x))

dryadic :: Int -> [Int]
dryadic n = map (1+) (tail (reverse(bin (n+1)))) where bin x = if (x == 1) then [1] else if (x `mod` 2 == 0) then 0 : bin (x `div` 2) else 1 : bin (x `div` 2)
