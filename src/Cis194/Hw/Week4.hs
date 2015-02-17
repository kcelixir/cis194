module Cis194.Hw.Week4 where

-- Exercise 1 --

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate collatz
  where collatz x = if even x then div x 2 else x*3+1

-- Exercise 2 --

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree a = foldr addTree Leaf a

addTree :: a -> Tree a -> Tree a
addTree x Leaf = Node 0 Leaf x Leaf
addTree x (Node _ l y r) = Node nh nl y nr
  where
    (nl,nr) | (h l) < (h r) = ((addTree x l), r)
            | otherwise     = (l, (addTree x r))
    nh = (max (h nl) (h nr)) + 1

--height
h Leaf = -1
h (Node x _ _ _) = x

-- Exercise 3 --

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4 --

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2:[2*x+1 | x<-[1..n], notElem x s]
    where s = [i+j+2*i*j | j <-[1..n], i<-[1..j]]

