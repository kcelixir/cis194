module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x
  | x < 1 = []
  | x < 10 = [x]
  | otherwise = toDigits(x `div` 10) ++ [(x `mod` 10 ) ] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 1 = []
  | x < 10 = [x]
  | otherwise = (x `mod` 10) : toDigitsRev(x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : ( y * 2 ) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ys) = (x `mod` 10) + (x `div` 10) + sumDigits ys

validate :: Integer -> Bool
validate x
  | sumDigits(doubleEveryOther(toDigitsRev x)) `mod` 10 == 0 = True
  | otherwise = False

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi i a b c = (hanoi (i-1) a c b) ++ [(a,b)] ++ (hanoi (i-1) c b a) 

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
