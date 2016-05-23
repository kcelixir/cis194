module Cis194.Hw.Golf where

import Data.List

-----------------------------------------------------------------------------
-- hopscotch
-- . output a list of lists, where each element is the list of n-skips
--   from the source list
--    ex: [1,2,3,4] -> [[1,2,3,4],[2,4],[3],[4]]
-- . this list contains n elements, so iterate i from 1 to n
-- . iterate j over each multiple of i up to n, and use drop j-1 to skip
--   over i elements at a time, creating a list of sublists, each one
--   headed by the desired element
--    ex: [[[1,2,3,4],[2,3,4],[3,4],[4]],[[2,3,4],[4]],[[3,4]],[[4]]]
-- . take the heads of each sublist to select the correct output
-----------------------------------------------------------------------------
skips :: [a] -> [[a]]
skips s = [map head h |
           i <- [1..length s],
           h <- [[drop (j - 1) s | j <- [i,i*2..length s]]]]

-----------------------------------------------------------------------------
-- local maxima
-- . select the elements of the list that are strictly greater than both
--   their left and right elements
--    ex:  [2,9,5,6,1] -> [9,6]
-- . zip up the respective nth, n+1th, n+2th elements from the list via drop
-- . select the middle of each triplet where it is strictly greater
-----------------------------------------------------------------------------
m :: (Integer, Integer, Integer) -> [Integer] -> [Integer]
m (x, y, z) l
   | x < y && y > z = y:l
   | True = l

localMaxima :: [Integer] -> [Integer]
localMaxima s = foldr m [] $ zip3 s (drop 1 s) (drop 2 s)

-----------------------------------------------------------------------------
-- histogram
-- . compute a textual histogram of a list of integers
--    ex: [1,4,5,4,6,6,3,4,2,4,9] ->
--            *
--            *
--            * *
--         ******  *
--        ==========
--        0123456789
-- . fill in each line of output by mapping each of [0..9] to an indicator
--   of where the number exists in the source list
--    ex: [1,1,2,4,8,8,8] -> " ** *   * "
-- . for each output line, remove one instance of 0-9 from the list
--    ex: [1,1,2,4,8,8,8] -> [1,2,8,8]
-- . recurse until the source list is empty
-----------------------------------------------------------------------------
h :: [Integer] -> String
h [] = ""
h l  = h (foldr delete l [0..9]) ++
       [if elem i l then '*' else ' ' | i <- [0..9]] ++
       "\n"

histogram :: [Integer] -> String
histogram l = h l ++ "==========\n0123456789\n"


-- skipss=[mapheadh|i<-[1..lengths],h<-[[drop(j-1)s|j<-[i,i*2..lengths]]]]
-- m(x,y,z)l|x<y&&y>z=y:l|True=llocalMaximas=foldrm[]$zip3s(drop1s)(drop2s)
-- h[]=""hl=h(foldrdeletel[0..9])++[ifelemilthen'*'else''|i<-[0..9]]++"\n"histograml=hl++"==========\n0123456789\n"
-- 71+72+112=255
