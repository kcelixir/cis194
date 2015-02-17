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
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate (\n -> if even n then n `div` 2 else 3*n+1)

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert a Leaf = Node 1 Leaf a Leaf
    insert a (Node _ left b right) | a > b = balance $ Node 1 left b (insert a right)
    insert a (Node _ left b right) = balance $ Node 1 (insert a left) b right
    balance n@(Node _ left v right)
      | depth left > depth right + 1 = Node (depth left) (leftSide left) (value left) (Node (depth left - 1) (rightSide left) v right)
      | depth left < depth right - 1 = Node (depth right) (Node (depth right - 1) left v (leftSide right)) (value right) (rightSide right)
      | otherwise = Node ((1 + max (depth left) (depth right))) left v right
    value (Node _ _ value _) = value
    leftSide (Node _ left _ _) = left
    rightSide (Node _ _ _ right) = right
    depth (Node depth _ _ _) = depth
    depth Leaf = 0

xor :: [Bool] -> Bool
xor = foldr (/=) False . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x l -> (f x) : l) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x + 1 | x <- [1..n], not (elem x witnesses)]
  where witnesses = [i + j + 2*i*j | j <-[1..n], i <- [1..j]]
