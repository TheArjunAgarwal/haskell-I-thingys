{-double x = x + x
quadruple x = double (double x )
factorial n = product [1..n]
average ns = sum ns `div` length ns
-}
{- import Data.Char (ord, chr)
import Distribution.Compat.Lens (_1)

n = a  `div` (length xs)
        where
                a = 10
                xs = [1, 2, 3, 4, 5]

odds :: Int -> [Int]
odds n = map f [0..(n-1)]
        where f x = x * 2 + 1


odds' n = map (\x -> x * 2 + 1) [0..(n-1)]

isprime n = not (any (\x ->n `mod` x == 0) [2..(n-1)])

halve :: [a] -> ([a], [a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

safetail xs = if null xs then [] else tail xs

safetail2 xs 
        | null xs =[]
        | otherwise = tail xs

safetail3 [] = []
safetail3 (_: xs) = xs

sos n = sum [x^2 | x <- [1..n]]
rep n a = [a | x<-[1..n]]
pyth n = [(x,y,z) | x <-[1..n], y <-[1..n] , y>x, z <-[1..n], z>x, z>y, x^2+y^2==z^2]
factors n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]
perfy n = [x | x <- [1..n], sum (factors x) == x]
euler n = sum [x | x<- [1..n], x `mod` 3 == 0 || x `mod` 5 == 0]
sp xs ys = sum [x*y | (x,y) <- zip xs ys]
choose n r = if r == 0 then 0 else (product [(n-r+1)..n]) `div` (product [1..r])
primesum n = sum [x | x <- [1..n], not(any (\a -> x `mod` a == 0) [2..(x-1)] )] - 1

leftrotate :: Integer -> Integer
leftrotate n = read(last (show n) : init(show n))
coco :: Integer -> Integer
coco n = if n == 1 then 0 else (coco(colletz n) + 1) where 
        colletz n = if n `mod` 2 == 0 then n `div` 2 else 3*n+1
ilog :: Integer -> Integer -> Integer
ilog b n = if n < b then 0 else (ilog b (n `div` b)) + 1

pow n 0 = 1
pow n k = n * (pow n (k-1))

ad :: [Bool] -> Bool
ad [x] = x
ad (x:xs) = x && (ad xs)

cc :: [[a]] -> [a]
cc [] = []
cc (x: xs) = x ++ cc (xs)

rc :: Int -> Int -> [Int]
rc n 0 = []
rc n r = [n] ++ (rc n (r-1))

sn :: [a] -> Int -> a
sn (x:xs) 0 = x
sn (x:xs) n = sn (xs) (n-1)

el :: Eq a => a -> [a] -> Bool
el a [] = False
el a (x:xs) = (a == x) || (el a xs)

mi :: Ord a => a -> [a] -> [a]
mi a [] = [a]
mi a (x:xs) = if a < x 
              then a : x : xs 
              else x : mi a xs

merge1 :: Ord a => [a] -> [a] -> [a]
merge1 [] xs = xs
merge1 xs [] = xs
merge1 (x:xs) ys = merge1 xs (mi x ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort a = merge1 (msort( fst (halve a))) (msort( snd (halve a) ))

sum1 [] = 0
sum1 (x:xs) = x + sum1 xs -}

{- take1 :: Integer -> [a] -> [a]
take1 0 ys = []
take1 n (x:xs) = x : take1 (n-1) xs

lasti :: [a] -> a
lasti [a] = a
lasti (x:xs) = last xs

alli :: (a -> Bool) -> [a] -> Bool
alli f  = foldr ((&&) . f) True 

anyi :: (a -> Bool) -> [a] -> Bool
anyi f = foldr ((&&) . f) False

takeWhilei :: (a -> Bool) -> [a] -> [a]
takeWhilei f xs = [x | x <- xs, f x]
-- takeWhilei f = foldr ((++) . (\a -> if f a then [a] else [])) [] 
--}
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{--
mapi :: (a -> b) -> [a] -> [b]
mapi f= foldr (\x -> \n -> f x : n) []

filteri :: (a -> Bool) -> [a] -> [a]
filteri f = foldr (\x -> \n -> if f x then x:n else n) []

dec2int :: [Int] -> Int
dec2int = foldl (\n d -> 10 * n + d) 0

curryi :: ((a,b) -> c) -> a->b->c
curryi f = (\a -> \b -> f (a,b)) -} 

poweri :: Integer -> Integer -> Integer
poweri n m 
        | m == 0 = 1
        | m > 0 = n * (poweri n (m-1))


-- Take a number and add it to its reverse and check if it is a palindrome. Else reverse and add.
palimake :: Integer -> Integer -> (Integer, Integer)
palimake n d  
        | show n == reverse(show n) = (n,d) 
        | (293<d && d<2880) = palimake (n + read(reverse(show n))) (d+1) 
        | otherwise = (0,(-1))

beep :: IO ()
beep = putStr "\BEL"
cls :: IO ()
cls = putStr "\ESC[2J"

main :: IO ()
main = do
    putStr "I am Kulkarni Bot. What did you use in proof? "
    tt <- getLine
    if tt /= "Natural Numbers" && tt /= "Logic" && tt /= "Set Theory"
    then do
        putStrLn $ "Huh, what is " ++ tt ++ "? Prove that it exists!"
        main
    else putStrLn "I guess that works" 

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x , y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat a b = do {goto a;
 putStrLn b}

seqn :: [IO a ] -> IO ()
seqn [] = return ()
seqn (a : as) = do {a;
seqn as}


width :: Int
width = 10
height :: Int
height = 10
type Board = [Pos]
glider :: Board
glider = [ (4, 2), (2, 3), (4, 3),(3, 4), (4, 4) ]
showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b ]
isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)
wrap :: Pos -> Pos
wrap (x , y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)
neighbs :: Pos -> [Pos]
neighbs (x , y) = map wrap [ (x - 1, y - 1),(x , y - 1), (x + 1, y - 1),(x - 1, y), (x + 1, y),(x - 1, y + 1), (x , y + 1),(x + 1, y + 1)]
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs
survivors :: Board -> [Pos ]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]
births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)), isEmpty b p, liveneighbs b p == 3]
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups(filter (/=x ) xs)
nextgen :: Board -> Board
nextgen b = survivors b ++ births b
wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]
life :: Board -> IO ()
life b = do {cls;
showcells b;
wait 5000;
life (nextgen b)}