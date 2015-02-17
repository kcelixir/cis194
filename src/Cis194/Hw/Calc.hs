module Cis194.Hw.Calc where

import  Cis194.Hw.ExprT
import  Cis194.Hw.Parser

-- Step 1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- Step 2

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Just x -> Just (eval x)
  _      -> Nothing

-- Step 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x   = Lit x
  add x y = Add x y
  mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

-- Step 4

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit x   = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x
    | x <= 0    = False
    | otherwise = True
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
  lit x   = MinMax x
  add x y = max x y
  mul x y = min x y

instance Expr Mod7 where
  lit x   = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 (x + y)
  mul x y = Mod7 (x * y)






