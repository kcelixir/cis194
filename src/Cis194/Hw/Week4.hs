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
addTree x (Node h l y r) = Node nh nl y nr
  where
    nh = (max (height nl) (height nr)) + 1
    (nl,nr) = if hl < hr then ((addTree x l),r) else (l,(addTree x r))
    (hl,hr) = (height l, height r)
    height Leaf = -1
    height (Node n _ _ _) = n

-- Exercise 3 --

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

-- Exercise 4 --
sieveSundaram :: Integer -> [Integer]
sieveSundaram _ = []

