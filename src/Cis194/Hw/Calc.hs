module Cis194.Hw.Calc where

import  Cis194.Hw.ExprT
import  Cis194.Hw.Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Just x -> Just (eval x)
  _      -> Nothing
