module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = (mod x 10) : toDigitsRev (div x 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x1:x2:xs) = (x1 * 2) : x2 : doubleEveryOther xs
doubleEveryOther x = x

sumDigits :: [Integer] -> Integer
sumDigits = (foldr (+) 0) -- or sum

composeInt :: Integer -> Integer
composeInt = sumDigits . concatMap toDigitsRev . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate x = (mod (composeInt x) 10) == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)
-- according to the HW:
-- length (hanoi 15 "a" "b" "c") == 32767

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
