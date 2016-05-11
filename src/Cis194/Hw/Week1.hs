module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x 
    | x <= 0 = []
    | otherwise = (mod x 10) : (toDigitsRev (div x 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = (reverse . doubleEverySecond . reverse)

doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond (x:y:xs) = x : (y * 2) : doubleEverySecond xs
doubleEverySecond x = x

sumDigits :: [Integer] -> Integer
sumDigits (x:xs) 
    | x < 10 = x + (sumDigits xs)
    | otherwise = (sumDigits (toDigitsRev x)) + sumDigits xs
sumDigits _ = 0

validate :: Integer -> Bool
validate x = (mod (sumDigits (doubleEveryOther (toDigits x))) 10) == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1 ) c b a

hanoiK :: Integer -> Integer
hanoiK n = (n - (floor (sqrt (( fromIntegral (2 * n)) + 1)))) + 1

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d = hanoi4 (hanoiK n) a c b d ++
                   hanoi (n - (hanoiK n)) a b d ++
                   hanoi4 (hanoiK n) c b a d
