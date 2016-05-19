module Cis194.Hw.Golf where

skips :: [a] -> [[a]]
skips s = [nth i s | i <- [1..length s]]

nth :: Int -> [a] -> [a]
nth n m = [c | (c, i) <- zip m (cycle [1..n]), i == n]

localMaxima :: [Integer] -> [Integer]
localMaxima s = [b | (a, b, c) <- zip3 s (drop 1 s) (drop 2 s), a < b && b > c]

histogram :: [Integer] -> String
histogram _ = ""
