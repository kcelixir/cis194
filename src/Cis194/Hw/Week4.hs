module Cis194.Hw.Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2'' :: Integer -> Integer
fun2'' n
   |    even n = n `div` 2
   | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate fun2''

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

treeFold :: a -> Tree a -> Tree a
treeFold a Leaf = Node 0 Leaf a Leaf
treeFold a (Node h l n r)
   | height l <= height r = let nl = treeFold a l
                            in Node (height nl + 1) nl n r
   |            otherwise = let nr = treeFold a r
                            in Node (height nr + 1) l n nr

foldTree :: [a] -> Tree a
foldTree = foldr treeFold Leaf

xor :: [Bool] -> Bool
xor = foldr (\x y -> (x || y) && not (x && y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n
   |     n < 2 = []
   | otherwise = 2:[2*x + 1 |
                    x <- [1..n],
                    not (x `elem` [i + j + 2*i*j |
                                   j <- [1..n],
                                   i <- [1..j],
                                   i + j + 2*i*j <= n])]
