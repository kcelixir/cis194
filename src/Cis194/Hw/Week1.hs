module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x | x < 1 = []
toDigits x | x < 10 = [x]
toDigits x = (toDigits $ x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse
  where
    doubleEveryOther' [] = []
    doubleEveryOther' (x : []) = [x]
    doubleEveryOther' (x : y : xs) = [x, 2*y] ++ doubleEveryOther' xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:xs) =  sumDigits xs + (sumDigits . toDigits) x

validate :: Integer -> Bool
validate x = 0 == ((`mod` 10) . sumDigits . doubleEveryOther . toDigits $ x)

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
