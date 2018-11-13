module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0 = []
toDigitsRev x = mod x 10 : toDigitsRev (div x 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = flipMap (zipWith (*) (cycle [1, 2]))

flipMap :: ([a] -> [b]) -> [a] -> [b]
flipMap f = reverse . f . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate card = rem (total card) 10 == 0
  where
    total = sumDigits . doubleEveryOther . toDigits

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
