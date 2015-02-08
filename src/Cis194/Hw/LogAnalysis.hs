{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import Cis194.Hw.Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":sev:time:msg) -> (LogMessage (Error (read sev)) (read time) (unwords msg))
  ("I":time:msg) -> (LogMessage Info (read time) (unwords msg))
  ("W":time:msg) -> (LogMessage Warning (read time) (unwords msg))
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left root right)
  | msgTime msg <= msgTime root = Node (insert msg left) root right
  | otherwise                   = Node left root (insert msg right)

msgTime :: LogMessage -> TimeStamp
msgTime (Unknown _) = error "No timestamp"
msgTime (LogMessage _ time _) = time

-- Exercise 3

build :: [LogMessage] -> MessageTree
build msgs = buildTree msgs Leaf

buildTree :: [LogMessage] -> MessageTree -> MessageTree
buildTree [] tree = tree
buildTree (msg:rest) tree = buildTree rest (insert msg tree)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map msgString . filter isRelevant . inOrder . build
-- whatWentWrong msgs = map msgString (filter isRelevant (inOrder (build msgs)))

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error sev) _ _)
  | sev >= 50 = True
  | otherwise = False
isRelevant (LogMessage _ _ _) = False
isRelevant (Unknown _) = False

msgString :: LogMessage -> String
msgString (LogMessage _ _ msg) = msg
msgString (Unknown msg) = msg

