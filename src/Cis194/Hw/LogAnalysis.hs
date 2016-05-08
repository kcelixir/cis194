{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
    "E":severity:time:message ->
      LogMessage (Error (read severity)) (read time) (unwords message)
    "I":time:message ->
      LogMessage Info (read time) (unwords message)
    "W":time:message ->
      LogMessage Warning (read time) (unwords message)
    message ->
      Unknown (unwords message)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert lm Leaf = Node Leaf lm Leaf
insert x@(LogMessage _ xTime _) (Node l y@(LogMessage _ yTime _) r)
  | xTime < yTime = Node (insert x l) y r
  | xTime > yTime = Node l y (insert x r)
insert _ t = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l x r) = inOrder l ++ [x] ++ inOrder r

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map messageText . filter isSevere . inOrder . build

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error severity) _ _) = severity >= 50
isSevere _ = False

messageText :: LogMessage -> String
messageText (LogMessage _ _ m) = m
messageText (Unknown m) = m
