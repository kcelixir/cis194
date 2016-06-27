{-# LANGUAGE FlexibleInstances #-}

module Cis194.Hw.Calc where

import           Cis194.Hw.ExprT
import           Cis194.Hw.Parser
import qualified Data.Map         as M

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
  lit = Mod7 . mod7
  add = plus
  mul = times


class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
             | Var String
             | VAdd VarExprT VarExprT
             | VMul VarExprT VarExprT
    deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var k = M.lookup k

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x
    | M.notMember x = Nothing
    | otherwise = M.lookup x
  add x y
    | _ M.notMember y = Nothing
    | M.notMember x $ _ = Nothing
    | otherwise = M.lookup x + M.lookup y
  mul x y
    | _ M.notMember y = Nothing
    | M.notMember x $ _ = Nothing
    | otherwise = M.lookup x * M.lookup y
