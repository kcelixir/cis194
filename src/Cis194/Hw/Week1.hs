module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x
   | x < 0     = []
   | x < 10    = [x]
   | otherwise = toDigits (x `div` 10) ++
                 toDigits (x `mod` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:zs) = x:(y * 2):(doubleEveryOther zs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
   | x < 10    = x + (sumDigits xs)
   | otherwise = (sumDigits (toDigits x)) + (sumDigits xs)

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigitsRev n))) `mod` 10 == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n - 1) a c b) ++
                [(a, b)] ++
                (hanoi (n - 1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d =
   let k = max (n - (floor (sqrt (fromIntegral (2 * n + 1))))) 0
   in (hanoi4 k a c b d) ++
      (hanoi (n - k) a b d) ++
      (hanoi4 k c b a d)
