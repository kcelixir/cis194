{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char

import Buffer
import Sized
import JoinList

----------
-- Ex 3 --
----------

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score 'a' = 1
score 'b' = 3
score 'c' = 3
score 'd' = 2
score 'e' = 1
score 'f' = 4
score 'g' = 2
score 'h' = 4
score 'i' = 1
score 'j' = 8
score 'k' = 5
score 'l' = 1
score 'm' = 3
score 'n' = 1
score 'o' = 1
score 'p' = 3
score 'q' = 10
score 'r' = 1
score 's' = 1
score 't' = 1
score 'u' = 1
score 'v' = 4
score 'w' = 4
score 'x' = 8
score 'y' = 4
score 'z' = 10
score _   = 0

scoreString :: String -> Score
scoreString = foldr mappend (Score 0) . map score

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l

----------
-- Ex 4 --
----------
jlCount :: (Sized m, Monoid m) => JoinList m a -> Int
jlCount Empty          = 0
jlCount (Single _ _)   = 1
jlCount (Append _ l r) = jlCount l + jlCount r

jlToString :: (Monoid m, Show a) => JoinList m a -> String
jlToString Empty          = ""
jlToString (Single _ a)   = show a
jlToString (Append _ l r) = jlToString l ++ jlToString r

lineToScoreSize :: String -> JoinList (Score, Size) String
lineToScoreSize s = Single (scoreString s, Size $ length s) s

instance Buffer (JoinList (Score, Size) String) where
  toString          = jlToString
  fromString s      = foldr (+++) Empty $ map lineToScoreSize $ lines s
  line              = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n + 1) b
  numLines          = jlCount
  value Empty                     = 0
  value (Single (Score x, _) _)   = x
  value (Append (Score x, _) _ _) = x
