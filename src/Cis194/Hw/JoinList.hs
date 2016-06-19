module JoinList where

import Data.Monoid

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

----------
-- Ex 1 --
----------
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ m = Append (tag a `mappend` tag m) a m

----------
-- Ex 2 --
----------
jlSize :: (Sized m, Monoid m) => JoinList m a -> Int
jlSize Empty          = 0
jlSize (Single m _)   = getSize $ size m
jlSize (Append m _ _) = getSize $ size m

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ 0 (Single _ a)   =  Just a
indexJ j (Append _ l r) =  if j < ls
                              then indexJ j l
                              else indexJ (j - ls) r
                           where ls = jlSize l
indexJ _ _              =  Nothing

dropJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
dropJ j s@(Single _ _)
   | j <= 0    = s
   | otherwise = Empty
dropJ j (Append _ l r)
   | j < ls    = Append lt ll r
   | otherwise = dropJ (j - ls) r
   where ls = jlSize l
         ll = dropJ j l
         lt = tag ll `mappend` tag r

takeJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
takeJ j s@(Single _ _)
   | j > 0     = s
   | otherwise = Empty
takeJ j (Append _ l r)
   | j > ls    = Append rt l rl
   | otherwise = takeJ j l
   where ls = jlSize l
         rl = takeJ (j - ls) r
         rt = tag l `mappend` tag rl
