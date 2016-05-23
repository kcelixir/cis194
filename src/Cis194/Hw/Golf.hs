module Cis194.Hw.Golf where
import Data.List

g = length

-- Map the nth over the length
skips :: [a] -> [[a]]
--skips s = map (h s) [1..g s]
skips s = map (\n -> [c | (c, i) <- zip s $ cycle [1..n], i == n]) [1..g s]

-- Every nth
-- zip with 1..nth and check for the ==
--h :: [a] -> Int -> [a]
--h m n = [c | (c, i) <- zip m (cycle [1..n]), i == n]

-- Zip all with offset ones and check
localMaxima :: [Integer] -> [Integer]
localMaxima s = [b | (a, b, c) <- zip3 s (tail s) (drop 2 s), a < b && b > c]

-- map 0..9 over the list and filter to just == and for each *
-- then pad to the max length and transpose and unlines
histogram :: [Integer] -> String
histogram l = unlines $ transpose [(replicate (b - g n) ' ') ++ n | n <- m]
    where m = [concat ["*" | x <-l, x == i] ++ "=" ++ show i| i <- [0..9]]
          b = maximum $ map g m
