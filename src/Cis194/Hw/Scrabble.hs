{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cis194.Hw.Scrabble where

import qualified Data.Set as Set

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score s) = s

score :: Char -> Score
score c
  | c `Set.member` one = 1
  | c `Set.member` two = 2
  | c `Set.member` three = 3
  | c `Set.member` four = 4
  | c `Set.member` five = 5
  | c `Set.member` eight = 8
  | c `Set.member` ten = 10
  | otherwise = 0
  where
    one = Set.fromList "aeioulnrst"
    two = Set.fromList "dg"
    three = Set.fromList "bcmp"
    four = Set.fromList "fhvwy"
    five = Set.fromList "k"
    eight = Set.fromList "jx"
    ten = Set.fromList "qz"

scoreString :: String -> Score
scoreString = foldr addScore (Score 0)
  where addScore c s = s `mappend` score c

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)
