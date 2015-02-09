module Cis194.Hw.Golf where

sks :: Int -> Int -> [a] -> [a]
sks _ _ [] = []
sks c i (x:xs)
    | (mod c i) == 0 = x:(sks (c + 1) i xs)
    | otherwise = sks (c + 1) i xs

skips :: [a] -> [[a]]
skips [] = []
skips x = let y = [1..(length x)]
         in (map (\i -> sks 1 i x) y)

localMaxima :: [Integer] -> [Integer]
localMaxima _ = []

histogram :: [Integer] -> String
histogram _ = ""
