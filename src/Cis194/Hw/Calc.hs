{-# LANGUAGE TypeSynonymInstances #-}

module Cis194.Hw.Calc where

import  Cis194.Hw.ExprT
import  Cis194.Hw.Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr str =
  case parseExp Lit Mul Add str of
    Just expr -> Just $ eval expr
    Nothing -> Nothing

newtype MinMax = MinMax Integer
  deriving (Eq, Show)

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = lit $ max x y
  mul (MinMax x) (MinMax y) = lit $ min x y

instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = lit $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = lit $ (x * y) `mod` 7

compile :: String -> Maybe Program
compile = case
