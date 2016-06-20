module Main where

import           Cis194.Hw.Editor
import           Cis194.Hw.JLBuffer

main = runEditor editor fromString :: JoinList (Score, Size) String $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
