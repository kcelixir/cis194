{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Cis194.Hw.JLBuffer where

import           Cis194.Hw.Buffer
import           Cis194.Hw.JoinList
import           Cis194.Hw.Scrabble
import           Cis194.Hw.Sized

instance Buffer (JoinList (Score, Size) String) where
  toString Empty    = ""
  toString (Single _ s) = s
  toString (Append _ l1 l2) = toString l1 ++ toString l2
  fromString s = Single (scoreString s, 1) s
  line = indexJ
  replaceLine n l b = takeJ (n -1) b +++ fromString l +++ dropJ (n + 1) b
  numLines     = getSize . snd . tag
  value        = getScore . fst . tag
