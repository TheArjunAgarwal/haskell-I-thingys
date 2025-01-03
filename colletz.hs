data BinTree = Nil | Node BinTree Int BinTree

multiplyAll :: BinTree -> Int
multiplyAll Nil = 0
multiplyAll (Node Nil x Nil) = x
multiplyAll (Node Nil x rtree) = x * multiplyAll rtree
multiplyAll (Node ltree x Nil) = x * multiplyAll ltree
multiplyAll (Node ltree x rtree) = x * multiplyAll rtree * multiplyAll ltree


enumThemFromTo :: Int -> Int -> Int -> [Int]
enumThemFromTo f s l = if (s < f) && (f < l) then [] else takeWhile (<= l) [f + n*d | n <- [0..]] where d = s-f

myReplicate :: Int -> a -> [a]
myReplicate i a = if i <=0 then [] else a:(myReplicate (i-1) a)

countFalse :: [Bool] -> Int
countFalse [] = 0
countFalse (x:xs) = if x then ld else 1 + ld where ld = countFalse xs

sortBools :: [Bool] -> [Bool]
sortBools [] = []
sortBools xs = replicate cf False ++ replicate ct True where 
    ct = length xs - cf
    cf = countFalse xs 

data BTree = Leaf Int | Fork BTree BTree
data DBTree = DLeaf Int | DFork DBTree (Int, Int) DBTree

leaves :: BTree -> [Int]
leaves (Leaf x) = [x]
leaves (Fork ltree rtree) = (leaves ltree) ++ (leaves rtree)

buildBTree :: [Int] -> BTree
buildBTree [x] = Leaf x
buildBTree xs = Fork (buildBTree lh) (buildBTree rh) where
        (lh,rh) = splitAt l xs
        l = length xs `div` 2

getDeco :: DBTree -> (Int, Int)
getDeco (DFork _ (x,y) _) = (x,y)

decorate :: BTree -> DBTree
decorate (Leaf x) = DLeaf x
decorate (Fork (Leaf l) (Leaf r)) = if l > r then DFork (DLeaf l) (l,r) (DLeaf r) else DFork (DLeaf l) (r,l) (DLeaf r)
decorate (Fork ltree (Leaf r)) 
    |r > big =DFork (dl) (r, small) (DLeaf r)
    |r < small = DFork (dl) (big, r) (DLeaf r)
    |otherwise = DFork dl (big, small) (DLeaf r)
        where
            (big, small) = getDeco dl
            dl = decorate ltree

decorate (Fork (Leaf l) rtree) 
    |l > big =DFork (DLeaf l) (l, small) (dr)
    |l < small = DFork (DLeaf l) (big, l) (dr)
    |otherwise = DFork (DLeaf l) (big, small) (dr)
        where
            (big, small) = getDeco dr
            dr = decorate rtree


decorate (Fork ltree rtree) = DFork dl (nb, ns) dr
    where
        (bigl, smalll) = getDeco dl
        (bigr, smallr) = getDeco dr
        nb = max (bigl, bigr)
        ns = max (smalll, smallr)
        dr = decorate rtree
        dl = decorate ltree
