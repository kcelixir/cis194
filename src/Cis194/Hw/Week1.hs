module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

-- Exercise 1 --
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0	= []
  | n < 10	= [n]
  | otherwise	= n `mod` 10 : (toDigitsRev (n `div` 10))

-- Exercise 2 --
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[])= [x]
doubleEveryOtherRev (x:(y:zs)) = (x) : y*2 : doubleEveryOtherRev zs

doubleEveryOther n = reverse (doubleEveryOtherRev (reverse n))

-- Exercise 3 --
sumDigitsN :: Integer -> Integer
sumDigitsN n = sum (toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = sumDigitsN x
sumDigits (x:(ys)) = sumDigitsN x + sumDigits ys

-- Exercise 4 --
validate :: Integer -> Bool
validate n
  | (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0 = True
  | otherwise = False

-- Exercise 5 (Hanoi) --
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0	= []
  | n == 1	= [(a,b)]
  | n >= 2	= hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- Exercise 6 (Hanoi 4 peg) --
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n == 1	= [(a,b)]
  | n == 2	= hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
  | n == 3	= hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
