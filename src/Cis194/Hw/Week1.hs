module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x | x < 0 = []
toDigits 0         = []
toDigits x         = toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate x = rem (total x) 10 == 0
  where
    total = sumDigits . doubleEveryOther . toDigits

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x source dest spare =
  hanoi (x-1) source spare dest ++
  [(source, dest)] ++
  hanoi (x-1) spare dest source

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _         = []
hanoi4 1 source dest _ _ = [(source, dest)]
hanoi4 d source dest a b =
  hanoi4 (d `quot` 2) source a dest b ++
  hanoi  (d - (d `quot` 2)) source dest b ++
  hanoi4 (d `quot` 2) a dest source b
