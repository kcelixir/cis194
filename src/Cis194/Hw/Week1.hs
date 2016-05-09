-- double every other digit (starting from right)
-- sum the result for the entire sequence
-- mod 10
-- == 0

:m +Test.QuickCheck
:l src/Cis194/Hw/Week1.hs

let toDigits :: Integer -> [Integer]
toDigits d = map (\x -> read x :: Integer) [(show d)]

let toDigitsRev :: Integer -> [Integer]
toDigitsRev d = map (\x -> read x :: Integer) (reverse [(show d)])

let doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []            = []
doubleEveryOther (d1:d0:[])    = (d1 * 2)
doubleEveryOther (ds:d1:d0:[]) = (d1 * 2) : doubleEveryOther ds

let sumDigits :: [Integer] -> Integer
sumDigits d = sum d

let validate :: Integer -> Bool
validate x = (mod (sumDigits (doubleEveryOther (toDigits x)))) == 0
