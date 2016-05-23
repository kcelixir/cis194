{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s =
   case words s of
      ("I":t:ms)   -> LogMessage Info (read t) (unwords ms)
      ("W":t:ms)   -> LogMessage Warning (read t) (unwords ms)
      ("E":e:t:ms) -> LogMessage (Error (read e)) (read t) (unwords ms)
      _            -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf        = Node Leaf m Leaf
insert m t =
   if ms < ts
      then Node (insert m l) tm r
      else Node l tm (insert m r)
   where LogMessage _ ms _               = m
         Node l tm@(LogMessage _ ts _) r = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms =
   [m | (LogMessage _ _ m) <- inOrder $ build ms]
