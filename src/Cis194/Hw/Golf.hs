module Cis194.Hw.Golf where

import           Data.List

-- skips takes a 1-indexed list the same length as the input list and maps over
-- it to produce a result with the same number of items as the original. Each
-- index in that list is passed through a list comprehension consisting of the
-- input list zipped with its indices. The indices are modded against the
-- element being mapped; only the elements at indices with a modulus of 0 are
-- kept.
skips :: [a] -> [[a]]
skips l = map (\x -> [i | (i, c) <- zip l [1..length l], mod c x == 0])  [1..length l]

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
