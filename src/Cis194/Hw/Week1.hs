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
hanoi 0 _ _ _ = []
hanoi n s t a = next n s a t ++
     [(s, t)] ++
     next n a t s
     where
       next = hanoi . subtract 1

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n s e i a
  | n <= 2 = hanoi n s e i
  | otherwise = next n s a e i ++
     hanoi4 2 s e i a ++
     next n a e i s
     where
       next = hanoi4 . subtract 2
