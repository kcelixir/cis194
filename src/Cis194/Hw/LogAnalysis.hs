{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
    "E":i:ws -> buildMessage (Error (read i)) ws
    "I":ws -> buildMessage Info ws
    "W":ws -> buildMessage Warning ws
    _ -> Unknown s

buildMessage :: MessageType -> [String] -> LogMessage
buildMessage m (t:ws) = LogMessage m (read t) (unwords ws)

parse :: String -> [LogMessage]
parse = map (parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert l Leaf = Node Leaf l Leaf
insert n@(LogMessage _ nt _) (Node l m@(LogMessage _ mt _) r)
  | nt < mt = Node (insert n l) m r
  | nt > mt = Node l m (insert n r)
insert _ t = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf . reverse

inOrder :: MessageTree -> [LogMessage]
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r
inOrder Leaf = []

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getString . filter isSever . inOrder . build

isSever :: LogMessage -> Bool
isSever (LogMessage (Error s) _ _) = s >= 50
isSever _ = False

getString :: LogMessage -> String
getString (LogMessage _ _ s) = s
getString (Unknown s) = s
