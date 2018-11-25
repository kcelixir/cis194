{-# LANGUAGE FlexibleInstances #-}
module Cis194.Hw.Fibonacci where

----------
-- Ex 1 --
----------

-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type:
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--
-- so that fib n computes the nth Fibonacci number Fn. Then, use fib to
-- define the infinite list of all Fibonacci numbers:
--
-- fibs1 :: [Integer]

fibs1 :: [Integer]
fibs1 = map fib [0..]

----------
-- Ex 2 --
----------

-- Define the infinite list:
--
-- fibs2 :: [Integer]
--
-- so that it has the same elements as fibs, but computing the first n
-- elements of fibs2 requires only O(n) addition operations. Be sure to
-- use standard recursion pattern(s) from Prelude, as appropriate.
--

fibs2 :: [Integer]
fibs2 = map (\(_, _, n) -> n)
  $ iterate (\(_, i, j) -> (i, j, i + j))
  (-1, 1, 0)

----------
-- Ex 3 --
----------

-- * Define a data type of polymorphic streams, Stream.
-- * Write a function to convert a Stream to an infinite list:
--
--   streamToList :: Stream a -> [a]

data Stream a = Cons a (Stream a)

streamToList :: Stream a  -> [a]
streamToList (Cons i s) = i : streamToList s

--
-- * Make your own instance of Show for Stream:
--
--   instance Show a => Show (Stream a) where
--     show ...
--
--   ...which works by showing only some prefix of a stream (say, the first
--   20 elements)

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

----------
-- Ex 4 --
----------

-- * Write a function:
--
-- streamRepeat :: a -> Stream a
--
-- ...which generates a stream containing infinitely many copies of the
-- given element
--

streamRepeat :: a -> Stream a
streamRepeat i = Cons i $ streamRepeat i

-- * Write a function:
--
-- streamMap :: (a -> b) -> Stream a -> Stream b
--
-- ...which applies a function to every element of a Stream
--

instance Functor Stream where
  fmap f (Cons i s) = Cons (f i) $ fmap f s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

-- * Write a function:
--
-- streamFromSeed :: (a -> a) -> a -> Stream a
--
-- ...which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a which
-- specifies how to transform the seed into a new seed, to be used for
-- generating the rest of the stream.

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed g i = Cons i $ streamFromSeed g $ g i

----------
-- Ex 5 --
----------

-- * Define the stream:
--
-- nats :: Stream Integer
--
-- ...which contains the infinite list of natural numbers 0, 1, 2...
--

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- * Define the stream:
--
-- ruler :: Stream Integer
--
-- ...which corresponds to the ruler function:
--
-- 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
--
-- ...where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly divides n.
--
-- Hint: define a function interleaveStreams which alternates the
-- elements from two streams. Can you use this function to implement ruler
-- in a clever way that does not have to do any divisibility testing?

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons i s) t = Cons i (interleaveStreams t s)

ruler :: Stream Integer
ruler = rulerN 0
  where rulerN n = interleaveStreams (streamRepeat n) (rulerN (n + 1))
