module Cis194.Hw.Calc where

import           Cis194.Hw.ExprT
import           Cis194.Hw.Parser

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalParsed :: Maybe ExprT -> Maybe Integer
evalParsed Nothing = Nothing
evalParsed (Just x) = Just $ eval x

evalStr :: String -> Maybe Integer
evalStr s = evalParsed $ parseExp Lit Add Mul s

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
  lit x = x <= 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
mod7 :: Integer -> Integer
mod7 = (`mod` 7)
plus :: Mod7 -> Mod7 -> Mod7
plus (Mod7 x) (Mod7 y) = Mod7 (mod7 (x + y))
times :: Mod7 -> Mod7 -> Mod7
times (Mod7 x) (Mod7 y) = Mod7 (mod7 (x * y))

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add = plus
  mul = times
