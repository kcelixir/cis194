module Cis194.Hw.Golf where

import           Data.List

-- t stands for 'take every'. Given a list of a certain type and an integer,
-- it take every nth element and concatenates it with a recursive application
-- of take every on the list following that element.
t :: [a] -> Int -> [a]
t xs n
  | n == 0 = xs
  | n >= length xs = []
  | True = xs !! n : t (drop (n+1) xs) n

skips :: [a] -> [[a]]
skips xs = [t xs x | x <- [0..length xs-1]]

-- localMaxima creates a sliding window over the input list, stores each
-- window step as a list of triples, and uses a list comprehension to extract
-- the second element of the triple if it meets the local maximum criteria.
localMaxima :: [Integer] -> [Integer]
localMaxima l = [y | (x,y,z) <- (\x -> zip3 x (drop 1 x) (drop 2 x)) l, y > x && y > z]

-- histogram first sorts the input list, groups the elements to organize
-- them into rows, then transposes those rows into columns, which leaves the
-- elements with the most occurrences at the end of the resulting list of lists.
-- The nested list is reversed to give the histogram its shape. Once that
-- initial processing is done, the nested list (now a list of histogram rows)
-- is mapped over using a list comprehension that outputs the desired character:
-- an asterisk if the current index is present in the row or a space if it is
-- absent. The list, now representing rows of the completed histogram, is
-- combined with two more lists: 10 occurrences of '=' and the characters for
-- '0' through '9'. The histogram rows, the separator row, and the legend row
-- are combined with 'unlines' to produce the final output.
histogram :: [Integer] -> String
histogram l = unlines (map (\x -> [if elem i x then '*' else ' ' | i <- [0..9]])
  (reverse (transpose (group (sort l)))) ++
  [take 10 ['=','='..], ['0'..'9']])
