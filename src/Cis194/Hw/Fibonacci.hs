{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Cis194.Hw.Fibonacci where

import Data.List

----------
-- Ex 1 --
----------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

----------
-- Ex 2 --
----------
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x + y)) (0, 1)

----------
-- Ex 3 --
----------
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

instance Show a => Show (Stream a) where
   show = show . take 20 . streamToList

----------
-- Ex 4 --
----------
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f $ f x

----------
-- Ex 5 --
----------
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStream :: (Stream Integer) -> (Stream Integer) -> (Stream Integer)
interleaveStream (Cons x xs) ys = Cons x $ interleaveStream ys xs

ruler :: Stream Integer
ruler = foldr1 interleaveStream $ map streamRepeat [0..]

----------
-- Ex 6 --
----------
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
   fromInteger n               = Cons n $ streamRepeat 0
   negate                      = streamMap (*(-1))
   Cons a0 a' + Cons b0 b'     = Cons (a0 + b0) (a' + b')
   Cons a0 a' * b@(Cons b0 b') = Cons (a0 * b0) $
                                      (streamMap (*a0) b') + a' * b

instance Fractional (Stream Integer) where
   Cons a0 a' / Cons 0 b'          = Cons a0 a' / b'
   a@(Cons a0 a') / b@(Cons b0 b') = Cons (a0 `div` b0) $
                                          streamMap (flip div b0)
                                                    (a' - q * b')
                                     where q = a / b

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

----------
-- Ex 7 --
----------
data Matrix = R2C2 Integer Integer
                   Integer Integer
   deriving Show

instance Num Matrix where
   R2C2 a b c d * R2C2 w x y z = R2C2 (a*w + b*y) (a*x + b*z)
                                      (c*w + d*y) (c*x + d*z)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 x = f where R2C2 f _ _ _ = (R2C2 1 1 1 0) ^ (x - 1)
