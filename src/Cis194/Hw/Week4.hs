module Cis194.Hw.Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree _ = Leaf

xor :: [Bool] -> Bool
xor = foldl1 (\r x -> ((not r && x) || (r && not x)))

map' :: (a -> b) -> [a] -> [b]
map' _ _ = []

sieveSundaram :: Integer -> [Integer]
-- sieveSundaram n = filter (\x -> x `elem` [(2 * i ) + 1 | i <- [1..n], j <- [1..n], i <= j, i + j + (2 * i * j) > n]) [1..(2 * n) + 2]
-- sieveSundaram n = [(2 * i ) + 1 | j <- [1..n], i <- [j..n], i + j + (2 * i * j) > n]
