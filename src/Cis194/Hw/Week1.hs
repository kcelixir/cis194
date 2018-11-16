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
-- validate x = mod (sumDigits (doubleEveryOther $ toDigits x)) 10 == 0
validate x = mod ((sumDigits . doubleEveryOther . toDigits) x) 10 == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n - 1) c b a)


-- Produces sub-optimal solutions, i.e. does not find the minimal number of moves for the 15-disc test.
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = (hanoi4 (n - 2) a c b d) ++ (hanoi4 2 a b d c) ++ (hanoi4 (n - 2) c b a d)
