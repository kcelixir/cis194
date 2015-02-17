module Cis194.Hw.Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' =  product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate iter
    where iter n = if even n
                   then n `div` 2
                   else (3 * n + 1)

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node _ l c r) = tree
  where hl = height l
        hr = height r 
        n = if hl <= hr then (insert a l) else (insert a r)
        h = height n + 1
        tree = if hl <= hr then Node h n c r else Node h l c n

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2:[2*x+1 | x <- [1..n], not $ elem x removed]
    where removed = [i + j + 2*i*j | j <- [1..n], i <- [1..j]]
