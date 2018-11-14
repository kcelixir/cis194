module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0     = (mod x 10) : (toDigitsRev $ div x 10)
  | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOther' $ reverse xs
doubleEveryOther' [] = []
doubleEveryOther' (x:xs) = x : doubleEveryOther'' xs
doubleEveryOther'' [] = []
doubleEveryOther'' (x:xs) = x * 2 : doubleEveryOther' xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . (map $ sum . toDigits)

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther $ toDigits x)) 10 == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
