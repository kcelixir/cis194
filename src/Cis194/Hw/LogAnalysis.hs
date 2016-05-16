{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import           Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
    ("E":sev:ts:msg) -> LogMessage (Error (read sev)) (read ts) (unwords msg)
    (level:ts:msg) | level == "W" || level == "I" ->
      LogMessage (msgType level) (read ts) (unwords msg)
      where msgType "W" = Warning
            msgType _ = Info
    _        -> Unknown s

parse :: String -> [LogMessage]
parse s = parseLines (lines s)
  where parseLines [] = []
        parseLines [x] = [parseMessage x]
        parseLines (x:xs) = parseMessage x : parseLines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert msg@(LogMessage _ ts _) (Node l p@(LogMessage _ ts2 _) r)
  | ts < ts2 = Node (insert msg l) p r
  | ts > ts2 = Node l p (insert msg r)
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
