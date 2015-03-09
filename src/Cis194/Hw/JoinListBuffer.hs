{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid

import Buffer
import JoinList
import Sized
import Scrabble

fromLine :: String -> JoinList (Score, Size) String
fromLine s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString     = serialize
  fromString   = foldr ((+++) . fromLine) Empty . lines
  line         = indexJ
  replaceLine n l b = (takeJ n b) +++ fromLine l +++ (dropJ (n+1) b)
  numLines     = listSize
  value        = getScore . fst . tag
