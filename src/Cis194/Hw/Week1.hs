module Cis194.Hw.Week1 where
import Data.Char

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x
  | x < 1 = []
  | otherwise = map (toInteger . digitToInt) (show x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = foldl (flip (:)) [] (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  let len = fromIntegral (length xs)
  in map (doubleMaybe len) (zip [0..] xs)

doubleMaybe :: Integer -> (Integer, Integer) -> Integer
doubleMaybe l (a, b)
  | isDivisibleBy 2 (l - a) = b * 2
  | otherwise = b

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy m x = mod x m == 0

sumDigits :: [Integer] -> Integer
sumDigits list = foldr (+) 0 (breakUpDigits list)

breakUpDigits :: [Integer] -> [Integer]
breakUpDigits list = list >>= toDigits

validate :: Integer -> Bool
validate = (isDivisibleBy 10) . sumDigits . doubleEveryOther . toDigits

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
