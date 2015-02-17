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
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' x = sum (iterate op x)
  where
    op n =
      if even n
      then
        n `div` 2
      else
        3 * n + 1

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = Leaf

xor :: [Bool] -> Bool
xor = foldl (\res x -> if x == True then not res else res) False

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) []  xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x + 1 | x <- [1..n], (notElem x deleted)]
  where
    deleted = [i + j + 2*i*j | i <- [1..n], j <- [i..n]]
