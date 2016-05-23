import           Data.Char

toDigits :: Integer -> [Integer]
toDigits n
  | n < 0 = []
  | otherwise = asIntegers (show n)

asIntegers :: String -> [Integer]
asIntegers s
  | s == "" = []
  | otherwise = toInteger (digitToInt (head s)) : asIntegers (tail s)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x,y] = [2 * x, y]
doubleEveryOther xs = do
  let h = length xs - 2
  let t = drop h xs
  doubleEveryOther (take h xs) ++ doubleEveryOther t

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum (toDigits x)
sumDigits (x:xs) = sumDigits [x] + sumDigits xs

validate :: Integer -> Bool
validate n
  | length (toDigits n) /= 16 = False
  | otherwise = do
    let ds = toDigits n
    let doubled = doubleEveryOther ds
    let summed = sumDigits doubled
    summed `mod` 10 == 0


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = (a, b)
  | otherwise = do
    let pegs = [1..n]
    hanoiPegs a b c [pegs,[],[]]

hanoiPegs :: Integer -> Peg -> Peg -> Peg -> [[Integer]] -> (Move, [[Integer]])
hanoiPegs n a b c pegs
  | n == last (pegs !! 0) = moveToTemp (head a) tail pegs


-- a,b a,c b,c
-- a,b a,c b,c a,b c,a
