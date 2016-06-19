module Main where

import Buffer
import Sized
import Editor
import JoinList
import Scrabble

main = runEditor editor $ (fromString "" :: JoinList (Score, Size) String)
