module Cis194.Hw.Golf where

skips :: [a] -> [[a]]
skips s = [nth i c | (c,i) <- zip (cycle [s]) [1..length s]]

nth :: Int -> [a] -> [a]
nth n m = take (floor (length m) `div` n) . concatMap (take 1) . iterate (drop n) $ m

localMaxima :: [Integer] -> [Integer]
localMaxima _ = []

histogram :: [Integer] -> String
histogram _ = ""
