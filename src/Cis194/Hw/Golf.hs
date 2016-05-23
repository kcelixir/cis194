module Cis194.Hw.Golf where

import Data.List

-----------------------------------------------------------------------------
-- hopscotch
skips :: [a] -> [[a]]
skips s = [map head h |
           i <- [1..length s],
           h <- [[drop (j - 1) s | j <- [i,i*2..length s]]]]

-----------------------------------------------------------------------------
-- local maxima
m :: (Integer, Integer, Integer) -> [Integer] -> [Integer]
m (x, y, z) l
   | x < y && y > z = y:l
   | True = l

localMaxima :: [Integer] -> [Integer]
localMaxima s = foldr m [] $ zip3 s (drop 1 s) (drop 2 s)

-----------------------------------------------------------------------------
-- histogram
h :: [Integer] -> String
h [] = ""
h l  = h (foldr delete l [0..9]) ++
       [if elem i l then '*' else ' ' | i <- [0..9]] ++
       "\n"

histogram :: [Integer] -> String
histogram l = h l ++ "==========\n0123456789\n"


--skipss=[mapheadh|i<-[1..lengths],h<-[map(\j->drop(j-1)s)[i,i*2..lengths]]]
--m(x,y,z)l|x<y&&y>z=y:l|True=llocalMaximas=foldrm[]$zip3s(drop1s)(drop2s)
--h[]=""hl=h(foldrdeletel[0..9])++[ifelemilthen'*'else''|i<-[0..9]]++"\n"histograml=hl++"==========\n0123456789\n"
-- 71+72+113=256
