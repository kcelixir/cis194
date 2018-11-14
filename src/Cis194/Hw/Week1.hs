module Cis194.Hw.Week1 where

-- import Cis194.Hw.AcceptHanoi

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
validate x = mod (sumDigits (doubleEveryOther $ toDigits x)) 10 == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []


putHeading :: String -> IO ()
putHeading s = putStrLn $ "\n\n===" ++ s ++ "===\n"

main = do
  putHeading "toDigits"
  putStrLn "toDigits 1234 `shouldBe` [1,2,3,4]"
  print $ toDigits 1234
  putStrLn "toDigits 0 `shouldBe` []"
  print $ toDigits 0
  putStrLn "toDigits (-1) `shouldBe` []"
  print $ toDigits (-1)

  putHeading "toDigitsRev"
  putStrLn "toDigitsRev 0 `shouldBe` []"
  print $ toDigitsRev 0
  putStrLn "toDigitsRev (-1) `shouldBe` []"
  print $ toDigitsRev (-1)
  putStrLn "toDigitsRev (-22222) `shouldBe` []"
  print $ toDigitsRev (-22222)
  putStrLn "toDigitsRev 123 `shouldBe` [3,2,1]"
  print $ toDigitsRev 123
  putStrLn "toDigitsRev 431 `shouldBe` [1,3,4]"
  print $ toDigitsRev 431
  putStrLn "toDigitsRev 12 `shouldBe` [2,1]"
  print $ toDigitsRev 12
  putStrLn "toDigitsRev 2 `shouldBe` [2]"
  print $ toDigitsRev 2

  putHeading "doubleEveryOther"
  putStrLn "doubleEveryOther [] `shouldBe` []"
  print $ doubleEveryOther []
  putStrLn "doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]"
  print $ doubleEveryOther [8,7,6,5]
  putStrLn "doubleEveryOther [1,2,3] `shouldBe` [1,4,3]"
  print $ doubleEveryOther [1,2,3]

  putHeading "sumDigits"
  putStrLn "sumDigits [] `shouldBe` 0"
  print $ sumDigits []
  putStrLn "sumDigits [16,7,12,5] `shouldBe` 22"
  print $ sumDigits [16,7,12,5]
  putStrLn "sumDigits [18,7,33,5] `shouldBe` 27"
  print $ sumDigits [18,7,33,5]

  putHeading "validate"
  putStrLn "validate 4012888888881881 `shouldBe` True"
  print $ validate 4012888888881881
  putStrLn "validate 4012888888881882 `shouldBe` False"
  print $ validate 4012888888881882

  putHeading "hanoi"
  putStrLn "hanoi 0 \"a\" \"b\" \"c\" `shouldBe` []"
  print $ hanoi 0 "a" "b" "c"
  putStrLn "hanoi 1 \"a\" \"b\" \"c\" `shouldBe` [(\"a\", \"b\")]"
  print $ hanoi 1 "a" "b" "c"
  putStrLn "(acceptHanoi3 hanoi 2) `shouldBe` Just (HanoiState3 [] [1..2] [])"
  -- print $ (acceptHanoi3 hanoi 2)
  -- putStrLn "(acceptHanoi3 hanoi 5) `shouldBe` Just (HanoiState3 [] [1..5] [])"
  -- print $ (acceptHanoi3 hanoi 5) `shouldBe` Just (HanoiState3 [] [1..5] [])
  -- putStrLn "(acceptHanoi3 hanoi 10) `shouldBe` Just (HanoiState3 [] [1..10] [])"
  -- print $ (acceptHanoi3 hanoi 10) `shouldBe` Just (HanoiState3 [] [1..10] [])
