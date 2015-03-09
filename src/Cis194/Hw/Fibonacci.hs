{-# LANGUAGE ParallelListComp #-}
module Cis194.Hw.Fibonacci where
import Data.List

----------
-- Ex 1 --
----------

-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type:

fib :: Integer -> Integer
fib x | (x<2) = x
      | otherwise = fib (x-1) + fib (x-2)

-- so that fib n computes the nth Fibonacci number Fn. Then, use fib to
-- define the infinite list of all Fibonacci numbers:

fibs1 :: [Integer]
fibs1 = [fib x | x <-[0..]]

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

fibs2 :: [Integer]
fibs2 = 0:1:[ a+b | a <- fibs2 | b <- (tail fibs2)]

-- I originally thought this should work:
-- fibs2 = foldl' f [0,1] [1..]
--   where f a n = a ++ [a!!n + a!!(n-1)]

-- Found this cool solution on the webz:
-- fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

-- Another:
-- fibs2 = 0:scanl (+) 1 fibs2

----------
-- Ex 3 --
----------

-- * Define a data type of polymorphic streams, Stream.

data Stream a = Stream a (Stream a)
 
-- * Write a function to convert a Stream to an infinite list:
--
--   streamToList :: Stream a -> [a]
--
streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

-- * Make your own instance of Show for Stream:
--
--   instance Show a => Show (Stream a) where
--     show ...
--
--   ...which works by showing only some prefix of a stream (say, the first
--   20 elements)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

----------
-- Ex 4 --
----------

-- * Write a function:
--
-- streamRepeat :: a -> Stream a
--
-- ...which generates a stream containing infinitely many copies of the
-- given element

streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

-- * Write a function:
--
-- streamMap :: (a -> b) -> Stream a -> Stream b
--
-- ...which applies a function to every element of a Stream

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

-- * Write a function:
--
-- streamFromSeed :: (a -> a) -> a -> Stream a
--
-- ...which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a which
-- specifies how to transform the seed into a new seed, to be used for
-- generating the rest of the stream.

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s $ streamFromSeed f $ f s

----------
-- Ex 5 --
----------

-- * Define the stream:
--
-- nats :: Stream Integer
--
-- ...which contains the infinite list of natural numbers 0, 1, 2...

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
interleaveStreams (Stream a as) bs = Stream a $ interleaveStreams bs as

ruler :: Stream Integer
ruler = f 0
  where f n = interleaveStreams (streamRepeat n) (f $ n+1)
