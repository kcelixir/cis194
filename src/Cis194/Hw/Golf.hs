module Cis194.Hw.Golf where
-- import Data.List.Split
-- dr n = map head . chunk n

skips :: [a] -> [[a]]
skips l = [(everynth n l) | n <- [1..(length l)]]

-- takes a list, l, and returns a new list with
-- only every nth value
everynth :: Int -> [a] -> [a]
everynth n l = [l !! (i-1) | i <- [n,2*n..(length l)]]

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:s)
  | y > x && y > z = [y] ++ (localMaxima (y:z:s))
  | otherwise    = localMaxima (y:z:s)

histogram :: [Integer] -> String
histogram = unlines . reverse . drawHist . hist

count x = length . filter (\y -> y == x)

hist :: [Integer] -> [Int]
hist a = [count n a | n <- [0..9]]

drawHist :: [Int] -> [String]
drawHist a =  ["==========\n0123456789"]++[drawRow n a | n <- [1..(maximum a)]]

drawRow :: Int -> [Int] -> String
drawRow n a = [if x >= n then '*' else ' ' | x <- a]
