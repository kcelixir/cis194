module Cis194.Hw.Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' _ = sum . takeWhile (>1) . iterate

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree _ = Leaf --foldr (\x acc -> ) Leaf 

xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> if x then acc + 1 else acc) 0

map' :: (a -> b) -> [a] -> [b]
map' _ _ = []

sieveSundaram :: Integer -> [Integer]
sieveSundaram _ = []
