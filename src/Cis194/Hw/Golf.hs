module Cis194.Hw.Golf where

-- Exercise 1
skips :: [a] -> [[a]]
skips s = map (\x -> takeEvery x s) [0..(length s)-1]

takeEvery :: Int -> [a] -> [a]
takeEvery n xs = case drop n xs of
  (y:ys) -> y : takeEvery n ys
  []     -> []

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima lst =
  if length lst >= 3
  then buildList' lst
  else []
  where
    buildList' (x:y:z:ls)
      | y > x && y > z = y : localMaxima(y:z:ls)
      | otherwise      = localMaxima(y:z:ls)

histogram :: [Integer] -> String
histogram _ = ""
