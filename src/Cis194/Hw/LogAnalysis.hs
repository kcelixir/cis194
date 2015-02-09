{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import Log

second :: String -> String
second s = (head (drop 1 (words s)))

makeTimestamp :: String -> Int
makeTimestamp s
    | head s == "E" = (read (head (drop 2 s)) :: Int)

makeErrorSev :: String -> Int
makeErrorSev s = (read (second s) :: Int)

parseMessage :: String -> LogMessage
parseMessage x@(t:s)
    | head s == "E" = LogMessage Error (makeErrorSev s)

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
