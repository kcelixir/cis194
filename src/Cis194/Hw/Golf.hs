module Cis194.Hw.Golf where
import Data.List

----------------
-- Exercise 1 --
----------------

-- skips applies function s for every possible n and concats resulting lists

skips :: [a] -> [[a]]
skips l = [(s n l) | n <- [1..(length l)]]

-- s takes a list, l, and returns a new list with only every nth value

s n l = [l !! (i-1) | i <- [n,2*n..(length l)]]

{- Function s can be shortened if we use chunk from the 'split' package:

import Data.List.Split
s n = map last . chunk n

-}

----------------
-- Exercise 2 --
----------------

{-
 Return a list of the local maximi for a list.

 Basic Approach:
  - Take first three elements, while labeling tail of list as 't'
  - include y in result if it is greater that neighbors x and z
  - recur with the tail, t

localMaxima :: [Integer] -> [Integer]
localMaxima (x:t@(y:z:_))
  | y > x && y > z = y : localMaxima t
  | otherwise      = localMaxima t
localMaxima _ = []

 A one line version is possible using list comprehension and tails (from Data.List).
  - tails takes list and progressively removes the head
  - we then filter that for local maximi
  - result is the filtered list
-}

localMaxima :: [Integer] -> [Integer]
localMaxima a = [y | (x:y:z:_) <- tails a, x < y && y > z]

----------------
-- Exercise 3 --
----------------

-- Count the frequencies,
-- Draw the histogram (as a list of strings)
-- Turn the list into newline delimited string

histogram :: [Integer] -> String
histogram = unlines . f . g

-- Count the frequencies of each number and store in a list

g a = [c n a | n <- [0..9]]

-- Counts how many of item x are in a list
-- first filter the list by the given item (using partial function!)
-- then count the length of the remaining list

c n = length . filter (==n)

-- Draw histogram using a list, a, of frequencies

f a =  [r n a | n <- [9,8..1], n <= maximum a] ++ ["==========\n0123456789"]

-- Draw row n of the histogram, using list, a, of frequencies

r n a = [if x >= n then '*' else ' ' | x <- a]

