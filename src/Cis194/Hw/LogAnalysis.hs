{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseTimestamp :: String -> Int
parseTimestamp (_:' ':i:' ':_) = read i

parseErrorInt :: String -> Int
parseErrorInt (_:i:' ':_) = read i

parseMessage :: String -> LogMessage
parseMessage s@('I':_) = Info (parseTimestamp s) s
parseMessage s@('W':_) = Warning (parseTimestamp s) s
parseMessage s@('E':_) = (Error (parseErrorInt s)) (parseTimestamp s) s

parse :: String -> [LogMessage]
parse _ = []

insert :: LogMessage -> MessageTree -> MessageTree
insert _ t = t

build :: [LogMessage] -> MessageTree
build _ = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder _ = []

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong _ = []
