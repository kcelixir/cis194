:l Week1.hs
:set prompt ">"

toDigits 1
toDigits 9
toDigits 0
toDigitsRev 102

doubleEveryOther [1, 2, 3, 4]

sumDigits [1, 2, 3, 45]

doubleEveryOther (toDigitsRev 4012888888881881)
sumDigits (doubleEveryOther (toDigitsRev 4012888888881881))

validate 4012888888881881
validate 4012888888881882

hanoi 3 "a" "b" "c"

hanoi4 3 "a" "b" "c" "d"
length (hanoi4 15 "a" "b" "c" "d")
