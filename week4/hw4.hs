{-# OPTIONS_GHC -Wall #-}

-- CIS 194: Homework 4

-- Exercise 1
--
---- Function 1
fun1 :: [Integer] -> Integer
fun1 = product . (map $ subtract 2) . filter even

---- Function 2
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate 
       (\x -> if even x then div x 2 else 3*x+1)

-- Exercise 2
--
-- Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree val Leaf        = Node 0 Leaf val Leaf
insertTree v (Node h l x r)
    | height l < height r = Node h (insertTree v l) x r
    | height l > height r = Node h l x (insertTree v r)
    | otherwise           = Node (1 + height iRight) l x iRight
        where
            height Leaf = -1
            height (Node ht _ _ _) = ht
            iRight = insertTree v r

-- Pretty printing tree
prettyTree :: (Show a) => Tree a -> String
prettyTree Leaf = ""
prettyTree (Node h l x r) = prettyPrint((Node h l x r),0)

prettyPrint :: (Show a) => (Tree a, Int)-> String
prettyPrint (Leaf,_) = ""
prettyPrint ((Node h l x r),z) = prettyPrint(r,z+6) ++ replicate z ' ' 
                                 ++ show x ++ "_" ++ show h ++ ['\n'] ++ prettyPrint(l,z+6)

-- Exercise 3
--
-- More folds!
--
-- Implement the xor function
xor :: [Bool] -> Bool
xor = foldr (\x y -> not x && y || x && not y) False

-- Implement map as fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- Implement foldl using foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

-- Exercise 4
--
-- Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) $ filter (\x -> notElem x $ sequences 1 n) [1..n]

sequences :: Integer -> Integer -> [Integer]
sequences i n
    | i <= j    = (map cond $ cartProd [i] [i..j]) ++ sequences (i+1) n
    | otherwise = []
        where j              = (n-i) `div` (1+2*i)
              cartProd xs ys = [(x,y) | x <-xs, y <- ys]
              cond           = \(x,y) -> x+y+2*x*y
