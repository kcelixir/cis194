module Cis194.Hw.Fibonacci where

--  Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = scanl (+) 0 (1:fibs2)


-- Exercise 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- ruler :: Stream Integer
-- ruler = foldr (\x acc -> interleaveStreams (streamRepeat x) (acc)) s [0..10]
--   where
--     s = streamRepeat 10

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = (Cons x (Cons y (interleaveStreams xs ys)))
