module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits = map (read . (:[])) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther1 :: [Integer] -> [Integer]
doubleEveryOther1 [] = []
doubleEveryOther1 (x:[]) = (x:[])
doubleEveryOther1 (x:(y:zs)) = x : (y * 2) : doubleEveryOther1 zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther1 . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

valid :: Integer -> Integer
valid = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate x
       | (valid x) `mod` 10 == 0 = True
       | (valid x) `mod` 10 /= 0 = False

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
