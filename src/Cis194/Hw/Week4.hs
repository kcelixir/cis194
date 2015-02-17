module Cis194.Hw.Week4 where


-- Ex 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl1 (*) . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then div x 2 else 3 * x + 1)

-- Ex 2

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) = Node 1 (foldTree (fst (halveList xs))) x (foldTree (snd (halveList xs)))
-- Doesn't work ^^^ the depth of the node isn't dynamic

halveList :: [a] -> ([a], [a])
halveList x = splitAt (div (length x) 2) x

-- Ex 3

xor :: [Bool] -> Bool
xor = foldl (\x _ -> x == False)  False . filter (== True)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- Ex 4 Had some help (thanks internet!)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) . filter fx $ [1..n]
    where fx x = not . any (\(i,j) -> i + j + 2*i*j == x) . filter (uncurry (<=)) $ cartProd [1..x]

-- My own unused helper function
fxn :: a -> Bool
fxn (x, y) = (x + y + 2 * x * y <= 100) && 1 <= x && x <= y

-- Modified cartProd
cartProd :: [a] -> [(a, a)]
cartProd xs = [(x,y) | x <- xs, y <- xs]

-- Extra stuff

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
