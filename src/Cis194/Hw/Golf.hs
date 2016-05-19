module Cis194.Hw.Golf where

skips :: [a] -> [[a]]
skips s = map (nth s) [1..length s]

nth :: [a] -> Int -> [a]
nth m n = [c | (c, i) <- zip m (cycle [1..n]), i == n]

localMaxima :: [Integer] -> [Integer]
localMaxima s = [b | (a, b, c) <- zip3 s (drop 1 s) (drop 2 s), a < b && b > c]

histogram :: [Integer] -> String
histogram _ = ""
