:set -i../../
:set prompt "> "
:l Golf.hs

skips "hello!"
localMaxima [2, 9, 5, 6, 1]
putStr $ "\n" ++ (histogram [1,4,5,4,6,6,3,4,2,4,9])

