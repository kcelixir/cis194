{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
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
fib n = fib (n-1) + fib (n-2)
--
-- so that fib n computes the nth Fibonacci number Fn. Then, use fib to
-- define the infinite list of all Fibonacci numbers:
--
-- fibs1 :: [Integer]

fibs1 :: [Integer]
fibs1 = map fib [1..]

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
fibs2 = 1 : fibs2' 1 1 where
  fibs2' x y = y : fibs2' y (x+y)

----------
-- Ex 3 --
----------

-- * Define a data type of polymorphic streams, Stream.
-- * Write a function to convert a Stream to an infinite list:
--
--   streamToList :: Stream a -> [a]
--
-- * Make your own instance of Show for Stream:
--
--   instance Show a => Show (Stream a) where
--     show ...
--
--   ...which works by showing only some prefix of a stream (say, the first
--   20 elements)

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show s = showStream 20 s

showStream :: Show a => Int -> Stream a -> String
showStream 0 _ = []
showStream 1 (Stream x _) = show x
showStream n (Stream x xs) = (show x) ++ "," ++ showStream (n-1) xs

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

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
streamFromSeed f x = Stream x $ streamFromSeed f (f x)

----------
-- Ex 5 --
----------

-- * Define the stream:
--
-- nats :: Stream Integer
--
-- ...which contains the infinite list of natural numbers 0, 1, 2...
--
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

----------
-- Ex 6 --
----------

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Stream n $ streamRepeat 0
  (+) (Stream a as) (Stream b bs) = Stream (a+b) (as + bs)
  negate (Stream a as) = Stream (negate a) (negate as)
  (*) (Stream a as) abs@(Stream b bs) = Stream (a*b) (streamMap (*a) bs + as*abs)

instance Fractional (Stream Integer) where
  (/) (Stream a as) (Stream b bs) | a == 0 && b == 0 = as / bs
  (/) ast@(Stream a as) bst@(Stream b bs) = Stream (div a b) (streamMap (`div` b) (as - (ast/bst)*bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

----------
-- Ex 7 --
----------

data Matrix = Matrix Integer Integer Integer Integer

instance Show (Matrix) where
  show (Matrix a b c d) = "[ " ++ (show a) ++ " " ++ (show b) ++ "\n  " ++ (show c) ++ " " ++ (show d) ++ " ]"


instance Num (Matrix) where
  (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = case (Matrix 1 1 1 0) ^ (n-1) of
  Matrix a b c d -> a
