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
localMaxima _ = []

histogram :: [Integer] -> String
histogram _ = ""
