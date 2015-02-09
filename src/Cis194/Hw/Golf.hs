module Cis194.Hw.Golf where

import Data.List
import Data.Ord

-- Exercise 1
skips :: [a] -> [[a]]
skips s = map (\x -> takeEvery' x s) [0..(length s)-1]
  where
    takeEvery' n xs = case drop n xs of
      (y:ys) -> y : takeEvery' n ys
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

-- Exercise 3
histogram :: [Int] -> String
histogram lst = rows lst ++ footer
  where
    footer = "==========\n0123456789\n"

rows :: [Int] -> String
rows lst = concatMap(\x -> (buildRow x f)) (reverse [1..m])
  where
    f = frequencies lst
    m = maxRow (frequencies lst)

frequencies :: [Int] -> [(Int, Int)]
-- frequencies = map (\xs -> (head xs, length xs)) . group . sort
frequencies lst = map (\x -> (x, (length (filter (==x) lst)))) [0..9]

maxRow :: [(Int, Int)] -> Int
maxRow lst = snd $ maximumBy (comparing snd) lst

buildRow :: Int -> [(Int, Int)] -> String
buildRow _ []     = "\n"
buildRow x (t:ts) = pickChar x t ++ buildRow x ts

pickChar :: Int -> (Int, Int) -> String
pickChar x t =
  if snd t >= x
  then "*"
  else " "
