{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
 "E":sev:ts:rest -> LogMessage (Error (read sev)) (read ts) (unwords rest)
 "I":ts:rest     -> LogMessage Info (read ts) (unwords rest)
 "W":ts:rest     -> LogMessage Warning (read ts) (unwords rest)
 _               -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- accessor for the timestamp of a log message
timeStamp :: LogMessage -> Int
timeStamp (LogMessage _ ts _ ) = ts
timeStamp (Unknown _) = -1

-- accessor for message of log messages
messageStr :: LogMessage -> String
messageStr (LogMessage _ _ m) = m
messageStr (Unknown _) = ""

-- accessor for error message's severity
severity :: LogMessage -> Int
severity (LogMessage (Error sev) _ _) = sev
severity _ = -1

-- looking for Errors of severity 50 or greater
severeEnough :: LogMessage -> Bool
severeEnough m = (severity m) >= 50

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert m (Node l lm r)
    | timeStamp m > timeStamp lm = Node l lm (insert m r)
    | otherwise = Node (insert m l) lm r

build :: [LogMessage] -> MessageTree
build [] = Leaf
build messages = foldl (\tree message -> insert message tree) Leaf messages

-- left first for in order traversal
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map messageStr $ inOrder $ build $ filter severeEnough messages
