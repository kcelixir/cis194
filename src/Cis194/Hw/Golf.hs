module Cis194.Hw.Golf where

import Data.List

skips :: [a] -> [[a]]
skips l = [ [l!!(i-1) | i <- [n, n*2..(length l)]] | n <- [1..(length l)] ]

localMaxima :: [Integer] -> [Integer]
localMaxima l = [y | (x:y:z:_) <- tails l, x < y && y > z]

histogram :: [Integer] -> String
histogram l = unlines $ reverse $  ["0123456789"] ++ ["=========="] ++ map concat (d l)

d :: [Integer] -> [[String]]
d l = [[if i >= j then "*" else " " | i <- c l] | j <- [1..maximum $ c l]]

c :: [Integer] -> [Int]
c l = map length [filter (==j) l | j <- [0..9]]
