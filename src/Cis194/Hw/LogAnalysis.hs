{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
    "E":i:t:ws -> buildMessage (Error (read i)) t ws
    "I":t:ws -> buildMessage Info t ws
    "W":t:ws -> buildMessage Warning t ws
    _ -> Unknown s

buildMessage :: MessageType -> String -> [String] -> LogMessage
buildMessage m t ws = LogMessage m (read t) (unwords ws)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert Unknown{} t = t
insert l Leaf = Node Leaf l Leaf
insert n@(LogMessage _ nt _) (Node l m@(LogMessage _ mt _) r)
  | nt < mt = Node (insert n l) m r
  | nt > mt = Node l m (insert n r)
insert _ t = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf . reverse

inOrder :: MessageTree -> [LogMessage]
inOrder t = go t []
  where
    go (Node l m r) = go l . (m:) . go r
    go Leaf = id

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getString . filter isSevere . inOrder . build

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error s) _ _) = s >= 50
isSevere _ = False

getString :: LogMessage -> String
getString (LogMessage _ _ s) = s
getString (Unknown s) = s
