{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log


parseMessage :: String -> LogMessage
parseMessage s | ('I' == s !! 0) = LogMessage Info (read (xs !! 1) :: Int) (unwords (drop 2 xs))
               | ('w' == s !! 0) = LogMessage Warning (read (xs !! 1) :: Int) (unwords (drop 2 xs))
               | ('E' == s !! 0) = LogMessage (Error (read (xs !! 1) :: Int)) (read (xs !! 2) :: Int) (unwords (drop 3 xs))
               | otherwise = Unknown s
               where xs = words s

parse :: String -> [LogMessage]
parse "" = []
parse s = [(parseMessage (head ss))] ++ parse (unlines (tail ss))
        where ss = lines s

getTimestamp :: LogMessage -> Int
getTimestamp (LogMessage _ t _) = t

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t 
insert m Leaf = Node Leaf m Leaf
insert m (Node l n r) | getTimestamp n > getTimestamp m = Node (insert m l) n r
                      | otherwise = Node l n (insert m r)

build :: [LogMessage] -> MessageTree
build (m:ms) = (insert m (build ms))
build _ = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node l n r) = inOrder l ++ [n] ++ inOrder r 
inOrder _ = []

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.

errorInt :: LogMessage -> Int
errorInt (LogMessage (Error i) _ _ ) = i
errorInt _ = -1

logString :: LogMessage -> String
logString (LogMessage _ _ s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map logString ( filter (\lm -> (errorInt lm) >= 50) ( inOrder (build ms)))
