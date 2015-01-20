-- Exercise 1
toDigits    :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- Exercise 2
doubleEveryOther  :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (double (reverse xs))

double :: [Integer] -> [Integer]
double [] = []
double [x] = [x]
double (x:(y:zs)) = x : (y*2) : double zs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concat (map toDigits xs))

-- Exercise 4
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =
  hanoi (n-1) a c b ++
  [(a, b)] ++
  hanoi (n-1) c b a