module Cis194.Hw.Golf where

skips :: [a] -> [[a]]
skips xs = [ [xs!!(x-1) | x <- [n,n*2..l]] | n <- [1..l] ]
    where l = length xs

maxima :: (Integer, Integer, Integer) -> [Integer]
maxima (x,y,z) | y > x && y > z = [y]
               | otherwise = []

part3 :: [Integer] -> [(Integer, Integer, Integer)]
part3 xs = zip3 xs (tail xs) (drop 2 xs)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = concatMap maxima (part3 xs)

counts :: [Integer] -> [Int]
counts xs = map (\n -> length $ filter (==n) xs) [0..9]

draw :: Int -> Int -> Char
draw n m
     | n >= m = '*'
     | otherwise = ' '

drawRow :: Int -> [Int] -> String
drawRow m = map (`draw` m)

histogram :: [Integer] -> String
histogram xs = unlines s ++ d
    where
      d = "==========\n0123456789\n"
      c = counts xs
      s = map (`drawRow` c) $ reverse [1..(maximum c)]
