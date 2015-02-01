{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  "I" : time : rest -> LogMessage Info (read time) (unwords rest)
  "W" : time : rest -> LogMessage Warning (read time) (unwords rest)
  "E" : code : time : rest -> LogMessage (Error (read code)) (read time) (unwords rest)
  _   -> Unknown s

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert message Leaf = Node Leaf message Leaf
insert (Unknown _) tree = tree
insert message (Node l m r) | timeFor message > timeFor m = Node l m (insert message r)
insert message (Node l m r) = Node (insert message l) m r

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ (m:[]) ++ (inOrder r)

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map messageFor) . (filter severeError) . inOrder . build

timeFor :: LogMessage -> Int
timeFor (Unknown _) = 0
timeFor (LogMessage _ time _) = time

messageFor :: LogMessage -> String
messageFor (Unknown message) = message
messageFor (LogMessage _ _ message) = message

severeError :: LogMessage -> Bool
severeError (LogMessage (Error severity) _ _) = severity >= 50
severeError _ = False
