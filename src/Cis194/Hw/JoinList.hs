module Cis194.Hw.JoinList where

import           Cis194.Hw.Scrabble
import           Cis194.Hw.Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty y = y
(+++) x Empty = x
(+++) a1 a2 = Append (mappend (tag a1) (tag a2)) a1 a2

sz :: (Sized b) => b -> Int
sz i = getSize (size i)

-- let lst = (Append (Size 3) (Append (Size 2) (Single (Size 1) 1) (Single (Size 1) 2)) (Single (Size 1) 3))

indexJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i l1 | i < 0 || i >= sz (tag l1) = Nothing
indexJ i (Single _ a) | i == 0 = Just a
indexJ i (Append _ l1 l2) | i > sz (tag l1) = indexJ (i - 1) l2
indexJ i (Append _ l1 _) | i < sz (tag l1) = indexJ (i - 1) l1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i l1 | i <= 0 = l1
dropJ i (Single _ _) | i > 0 = Empty
dropJ i (Append _ l1 l2) | i > sz (tag l1) = dropJ (i - sz (tag l1)) l2
dropJ i (Append _ l1 l2) | i < sz (tag l1) = Append (tag dropped `mappend` tag l2) dropped l2
  where dropped = dropJ i l1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i == 0 = Empty
takeJ _ l1@(Single _ _) = l1
takeJ i (Append _ l1 l2) | i >= sz (tag l1) = Append (tag l1 `mappend` tag taken) l1 taken
  where taken = takeJ (i - sz (tag l1)) l2
takeJ i (Append _ l1 _) | i < sz (tag l1) = takeJ i l1
takeJ _ _ = Empty

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
