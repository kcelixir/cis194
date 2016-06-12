:set -i../../
:set prompt "> "
:l Fibonacci.hs
-- :l ../../../test/Cis194/Hw/CalcSpec.hs

-- main

True

take 10 fibs2

foldr Cons Empty [1..1000]

streamRepeat 5

streamMap (+1) $ streamRepeat 3

streamFromSeed (+2) 0

nats

interleaveStream (streamRepeat 0) (streamRepeat 1)

ruler
