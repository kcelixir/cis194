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
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)


----------
-- Ex 3 --
----------

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons n ns) = n : streamToList ns

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

----------
-- Ex 4 --
----------

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons n ns) = Cons (f n) (streamMap f ns)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Cons n (streamFromSeed f (f n))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons n ns) m = Cons n (interleaveStreams m ns)

----------
-- Ex 5 --
----------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = foldr (interleaveStreams . streamRepeat) (streamRepeat 0) [0..]

----------
-- EX 6 --
----------

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate (Cons n ns) = Cons (negate n) (negate ns)
  (+) (Cons n ns) (Cons m ms) = Cons (n+m) (ns+ms)
  (*) (Cons n ns) b@(Cons m ms) = Cons (n*m) $ streamMap (n*) ms + (ns*b)

instance Fractional (Stream Integer) where
  (/) a@(Cons n ns) b@(Cons m ms) = Cons (n `div` m) $ streamMap (div m) (ns - (a/b)*ms)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
