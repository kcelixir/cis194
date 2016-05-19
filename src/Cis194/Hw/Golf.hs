module Cis194.Hw.Golf where

-- te stands for 'take every'. Given a list of a certain type and an integer,
-- it take every nth element and concatenates it with a recursive application
-- of take every on the list following that element.
te :: [a] -> Int -> [a]
te xs n
  | n == 0 = xs
  | n >= length xs = []
  | True = xs !! n : te (drop (n+1) xs) n

skips :: [a] -> [[a]]
skips xs = [te xs x | x <- [0..length xs-1]]

-- localMaxima creates a sliding window over the input list, stores each
-- window step as a list of triples, and uses a list comprehension to extract
-- the second element of the triple if it meets the local maximum criteria.
localMaxima :: [Integer] -> [Integer]
localMaxima l = [y | (x,y,z) <- (\x -> zip3 x (drop 1 x) (drop 2 x)) l, y > x && y > z]

histogram :: [Integer] -> String
histogram _ = ""
