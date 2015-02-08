module Cis194.Hw.Golf where
-- import Data.List.Split
-- dr n = map head . chunk n

----------------
-- Exercise 1 --
----------------
skips :: [a] -> [[a]]
skips l = [(s n l) | n <- [1..(length l)]]

s :: Int -> [a] -> [a]
-- takes a list, l, and returns a new list with
-- only every nth value
s n l = [l !! (i-1) | i <- [n,2*n..(length l)]]

----------------
-- Exercise 2 --
----------------
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:s)
  | y > x && y > z = [y] ++ (localMaxima (y:z:s))
  | otherwise    = localMaxima (y:z:s)

----------------
-- Exercise 3 --
----------------
histogram :: [Integer] -> String
histogram = unlines . f . g

g :: [Integer] -> [Int]
-- Count the frequencies of each number and store in a list
g a = [c n a | n <- [0..9]]

c :: Integer -> [Integer] -> Int
-- counts how many of item x are in a list
c x = length . filter (\y -> y == x)

f :: [Int] -> [String]
-- Draw histogram using a list, a, of frequencies
f a =  [r n a | n <- [9,8..1], n <= maximum a] 
    ++ ["==========\n0123456789"]

r :: Int -> [Int] -> String
-- Draw row n of the histogram, using list, a, of frequencies
r n a = [if x >= n then '*' else ' ' | x <- a]

