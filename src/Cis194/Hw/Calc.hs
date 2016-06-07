{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Cis194.Hw.Calc where

import Cis194.Hw.Parser
-- import Cis194.Hw.ExprT
import qualified Cis194.Hw.StackVM as StackVM
import qualified Data.Map as M

-- eval :: ExprT -> Integer
-- eval (Lit x) = x
-- eval (Add x y) = eval x + eval y
-- eval (Mul x y) = eval x * eval y

-- evalStr :: String -> Maybe Integer
-- evalStr str =
--   case parseExp Lit Mul Add str of
--     Just expr -> Just $ eval expr
--     Nothing -> Nothing

newtype MinMax = MinMax Integer
  deriving (Eq, Show)

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

data VarExprT = Var String
              | Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)

-- instance Expr ExprT where
--   lit = Lit
--   add = Add
--   mul = Mul

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

instance Expr StackVM.Program where
  lit x   = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _   = Just x
  add x y m = (+) <$> x m <*> y m
  mul x y m = (*) <$> x m <*> y m

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

withVars :: [(String, Integer)]
        -> (M.Map String Integer -> Maybe Integer)
        -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

test :: Maybe Integer
test = withVars [("x", 6), ("y", 3)] $ mul (add (lit 3) (var "x")) (var "y")
