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
  fromString   = appendList . lines where
    appendList [] = Empty
    appendList [s] = fromLine s
    appendList l = (appendList (take (half l) l)) +++ (appendList (drop (half l) l))
    half = (`div` 2) . length
  line         = indexJ
  replaceLine n l b = (takeJ n b) +++ fromLine l +++ (dropJ (n+1) b)
  numLines     = listSize
  value        = getScore . fst . tag
