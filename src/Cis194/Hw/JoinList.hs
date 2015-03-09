{-# LANGUAGE GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , FlexibleInstances
   #-}
module Cis194.Hw.JoinList where

import Data.Monoid
import Cis194.Hw.Sized

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- Ex 1 --

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x Empty = x
(+++) Empty x = x
(+++) x y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Ex 2 --

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ i x | i >= jlSize x = Nothing
indexJ 0 (Single _ x) = Just x
indexJ i (Append _ x y)
  | i < jlSize x = indexJ i x
  | otherwise    = indexJ (i-jlSize x) y

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x | n <= 0 = x
dropJ n x | n >= jlSize x = Empty
dropJ n (Append _ x y)
  | n <= jlSize x = (dropJ n x) +++ y
  | otherwise    = x +++ (dropJ (n-jlSize x) y)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ n x | n >= jlSize x = x
takeJ n (Append _ x y)
  | n <= jlSize x = (takeJ n x)
  | otherwise    = x +++ (takeJ (n-jlSize x) y)
