:set -i../../
:set prompt "> "
:l Fibonacci.hs
:l ../../../test/Cis194/Hw/FibonacciSpec.hs

main

take 10 fibs2

foldr Cons (streamRepeat 0) [1..1000]

streamRepeat 5

streamMap (+1) $ streamRepeat 3

streamFromSeed (+2) 0

nats

interleaveStream (streamRepeat 0) (streamRepeat 1)

ruler

fibs3

fib4 20
