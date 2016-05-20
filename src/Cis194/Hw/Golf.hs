module Cis194.Hw.Golf where

skips :: [a] -> [[a]]
skips s = map (h s) [1..length s]

h :: [a] -> Int -> [a]
h m n = [c | (c, i) <- zip m (cycle [1..n]), i == n]

localMaxima :: [Integer] -> [Integer]
localMaxima s = [b | (a, b, c) <- zip3 s (drop 1 s) (drop 2 s), a < b && b > c]

histogram :: [Integer] -> String
histogram l = unlines (map (t [(replicate (b - length n) ' ') ++ n | n <- m]) [1..b])
    where m = [concat ["*" | x <-l, x == i] ++ "=" ++ show i| i <- [0..9]]
          b = maximum (map length m)

t :: [String] -> Int -> String
t l i = [ head (h s i) | s <- l]
