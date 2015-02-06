module Cis194.Hw.Golf where

import Data.List

nthItem :: Int -> [a] -> [a]
nthItem n xs
    | length xs < n = []
    | otherwise = head (drop (n-1) xs) : nthItem n (drop n xs)

skips :: [a] -> [[a]]
skips xs = [nthItem x xs | x <- [1..(length xs)]]

maxima :: (Integer, Integer, Integer) -> [Integer]
maxima (x,y,z) | (y > x && y > z) = [y]
               | otherwise = []

part3 :: [Integer] -> [(Integer, Integer, Integer)]
part3 xs = (zip3 xs (tail xs) (drop 2 xs))

localMaxima :: [Integer] -> [Integer]
localMaxima xs = concatMap maxima (part3 xs)

counts :: [Integer] -> [Int]
counts xs = map (\n -> length $ filter (==n) xs) [0..9]

draw :: Int -> Int -> Char
draw n m
     | n >= m = '*'
     | otherwise = ' '

drawRow :: Int -> [Int] -> String
drawRow m xs = map (\x -> (draw x m)) xs

histogram :: [Integer] -> String
histogram xs = unlines s ++ defStr
    where
      defStr = "==========\n0123456789\n"
      cs = counts xs
      s = map (flip drawRow cs) $ reverse [1..(maximum cs)]
