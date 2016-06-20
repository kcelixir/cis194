{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Cis194.Hw.JoinList where

import Data.Monoid
import Cis194.Hw.Sized
import Cis194.Hw.Scrabble
import Cis194.Hw.Buffer
import Cis194.Hw.Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++):: Monoid m => JoinList m a ->  JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m
tag Empty          = mempty

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Append _ ll lr)
  | i < sizeJ ll      = indexJ i ll
  | otherwise         = indexJ (i - sizeJ ll) lr
indexJ 0 (Single _ a) = Just a
indexJ _ _            = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Append _ ll lr)
  | n < sizeJ ll     = dropJ n ll +++ lr
  | otherwise        = dropJ (n - sizeJ ll) lr
dropJ 0 (Single m a) = Single m a
dropJ _ _            = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Append _ ll lr)
  | n < sizeJ ll                = takeJ n ll
  | n == sizeJ ll               = ll
  | otherwise                   = ll +++ takeJ (n - sizeJ ll) lr
takeJ n jl@(Single _ _) | n > 0 = jl
takeJ _ _                       = Empty

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

createLine :: String -> JoinList (Score, Size) String
createLine s = Single (scoreString s, Size 1) s

createLines :: [String] -> JoinList (Score, Size) String
createLines = foldr ((+++) . createLine) Empty

instance Buffer (JoinList (Score, Size) String) where
  toString Empty            = ""
  toString (Single _ s)     = s
  toString (Append _ ll lr) = toString ll ++ toString lr

  fromString                = createLines . lines

  line                      = indexJ

  replaceLine n s jl        = takeJ n jl +++ createLine s +++ dropJ n jl

  numLines                  = sizeJ

  value                     = getScore . fst . tag

main = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
