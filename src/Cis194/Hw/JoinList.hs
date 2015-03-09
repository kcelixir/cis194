{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) la lb = Append (tag la <> tag lb) la lb

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _)  = m

-- Exercise 2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _          = Nothing
_      !!? i | i < 0  = Nothing
(x:xs) !!? 0          = Just x
(x:xs) !!? i          = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ (Empty)      = Nothing
indexJ i (Single _ a)
  | i == 0            = Just a
  | otherwise         = Nothing
indexJ i (Append m ll lr)
  | i < (sizeJ ll)    = indexJ i ll
  | otherwise         = indexJ (i - (sizeJ ll)) lr

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty       = Empty
dropJ i (Single m a)
  | i == 0          = Single m a
  | otherwise       = Empty
dropJ i (Append m ll lr)
  | i < (sizeJ ll)  = (dropJ i ll) +++ lr
  | otherwise       = dropJ (i - (sizeJ ll)) lr

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl@(Single m a)
  | i > 0     = jl
  | otherwise = Empty
takeJ i (Append m ll lr)
  | i < (sizeJ ll) = takeJ i ll
  | i == (sizeJ ll) = ll
  | otherwise = ll +++ takeJ (i - (sizeJ ll)) lr

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

createLine :: String -> JoinList (Score, Size) String
createLine s = Single (scoreString s, Size 1) s

createLines :: [String] -> JoinList (Score, Size) String
createLines = foldr (\l acc -> (createLine l) +++ acc) Empty

instance Buffer (JoinList (Score, Size) String) where
  toString (Empty)          = ""
  toString (Single m a)     = a
  toString (Append m ll lr) = (toString ll) ++ (toString lr)
  fromString = createLines . lines
  replaceLine i s jl = (takeJ i jl) +++ (createLine s) +++ (dropJ i jl)
  numLines = sizeJ
  value = getScore . fst . tag
  line = indexJ

main = runEditor editor $ createLines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
