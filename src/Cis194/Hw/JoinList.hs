module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

listSize :: (Sized b, Monoid b) => JoinList b a -> Int
listSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n jl | n < 0 || n >= listSize jl = Nothing
indexJ 0 (Single _ a) = Just a
indexJ n (Append s l r) | n < listSize l = indexJ n l
indexJ n (Append _ l r) = indexJ (n - listSize l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ n jl | n >= listSize jl = Empty
dropJ n (Single s a) = Single s a
dropJ n (Append _ l r) = dropJ n l +++ dropJ (n - listSize l) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl | n <= 0 = Empty
takeJ n jl | n >= listSize jl = jl
takeJ n (Single s a) = Single s a
takeJ n (Append _ l r) = takeJ n l +++ takeJ (n - listSize l) r
