module Cis194.Hw.Golf where
import Data.List

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
localMaxima [] = []
localMaxima (x:y:[]) = []
localMaxima (x:r@(y:z:zz))
            | (y > x) && (y > z) = y:(localMaxima r)
            | otherwise = localMaxima r

push :: Integer -> [Char]
push 1 = "*"
push x = (" ":push (x - 1))

punch :: [Integer] -> [String] -> [String]
punch (x:xs) z
    | length z == 9 = z
    | x > 0 = punch xs (z:(push (x - (length z))))

his :: [Integer] -> Integer -> [Integer]
his x y = (filter (\i -> i == y) x)

histogram :: [Integer] -> [[Integer]]
histogram x = let z = "==========\n0123456789\n"
                      -- y = [1..9]
              in
              (map (\i -> his x i) [1..9])
              
