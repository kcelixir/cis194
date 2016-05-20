module Cis194.Hw.Golf where

skips :: [a] -> [[a]]
skips s = map (nth s) [1..length s]

nth :: [a] -> Int -> [a]
nth m n = [c | (c, i) <- zip m (cycle [1..n]), i == n]

localMaxima :: [Integer] -> [Integer]
localMaxima s = [b | (a, b, c) <- zip3 s (drop 1 s) (drop 2 s), a < b && b > c]

histogram :: [Integer] -> String
histogram l = unlines (transpose [concat ((replicate (b - length n) " ") ++ n) | n <- m])
    where m = [["*" | x <-l, x == i] ++ ["=", show i]| i <- [0..9]]
          b = maximum (map length m)

transpose :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
