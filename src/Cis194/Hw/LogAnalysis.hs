{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseError :: String -> String -> [String] -> LogMessage
parseError severity time msg =
  LogMessage (Error (read severity)) (read time) (unwords msg)

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E" : severity : time : rest) -> parseError severity time rest
  ("I" : time : rest) -> LogMessage Info (read time) (unwords rest)
  ("W" : time : rest) -> LogMessage Warning (read time) (unwords rest)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left current right) =
  if (timestamp msg) > (timestamp current)
  then Node left current (insert msg right)
  else Node (insert msg left) current right

timestamp :: LogMessage -> TimeStamp
timestamp (LogMessage _ t _) = t
timestamp (Unknown _) = error "Unknown message has no timestamp"

build :: [LogMessage] -> MessageTree
build messages = foldl (flip insert) Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) =
  inOrder left ++ [msg] ++ inOrder right

message :: LogMessage -> String
message (Unknown s) = s
message (LogMessage _ _ s) = s

onlySevere :: LogMessage -> Bool
onlySevere (LogMessage (Error s) _ _) = s >= 50
onlySevere _ = False

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map message) . (filter onlySevere) . inOrder . build
