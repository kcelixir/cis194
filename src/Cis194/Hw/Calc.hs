{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Cis194.Hw.Calc where

import Cis194.Hw.ExprT
import Cis194.Hw.Parser
import qualified Cis194.Hw.StackVM as VM

-----------------------------------------------------------------------------
-- Exercise 1
-----------------------------------------------------------------------------
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-----------------------------------------------------------------------------
-- Exercise 2
-----------------------------------------------------------------------------
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
               Just e  -> Just $ eval e
               Nothing -> Nothing

-----------------------------------------------------------------------------
-- Exercise 3
-----------------------------------------------------------------------------
class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a

instance Expr ExprT where
   lit = Lit
   add = Add
   mul = Mul

-----------------------------------------------------------------------------
-- Exercise 4
-----------------------------------------------------------------------------
instance Expr Integer where
   lit = id
   add = (+)
   mul = (*)

instance Expr Bool where
   lit = (>0)
   add = (||)
   mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
   lit x                     = MinMax x
   add (MinMax x) (MinMax y) = MinMax $ max x y
   mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
   lit x = Mod7 $ x `mod` 7
   add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
   mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

-----------------------------------------------------------------------------
-- Exercise 5
-----------------------------------------------------------------------------
instance Expr VM.Program where
   lit x   = [VM.PushI x]
   add x y = x ++ y ++ [VM.Add]
   mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
