module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------
--1
lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit 0 = 0
dropLastDigit n = ( n - lastDigit n) `div` 10

toDigits :: Integer -> [Integer]
toDigits x = [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = xs

sumDigits :: [Integer] -> Integer
sumDigits _ = 0

validate :: Integer -> Bool
validate _ = False


---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
