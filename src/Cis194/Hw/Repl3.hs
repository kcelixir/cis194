:set -i../../
:set prompt "> "
:l Golf.hs
:l ../../../test/Cis194/Hw/GolfSpec.hs

main

skips "hello!"
localMaxima [2, 9, 5, 6, 1]
putStr $ "\n" ++ (histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9])

