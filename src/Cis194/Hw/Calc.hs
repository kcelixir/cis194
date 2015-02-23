{-# LANGUAGE FlexibleInstances #-}
module Cis194.Hw.Calc where 

import Cis194.Hw.ExprT
import Cis194.Hw.Parser
-- For more information on how to use Data.Map, follow the link:
-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
              Just e -> Just (eval e)
              Nothing -> Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit x = Lit x
    add x y = Add x y
    mul x y = Mul x y
                   
reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit x = MinMax x
    add (MinMax y) (MinMax z) = MinMax (max y z)
    mul (MinMax y) (MinMax z) = MinMax (min y z)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit x = Mod7 (mod x 7)
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

class HasVars a where
    var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String
  deriving (Eq, Show)

instance Expr VarExprT where
    lit x = VarLit x
    add x y = VarAdd x y
    mul x y = VarMul x y

instance HasVars VarExprT where
    var s = Var s

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = M.lookup s

-- only apply f if x and y are not nothing
maybeF :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeF f (Just x) (Just y) = Just (f x y)
maybeF _ _ _ = Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = \_ -> Just x
    add x y = \m -> maybeF (+) (x m) (y m)
    mul x y = \m -> maybeF (*) (x m) (y m)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

{-
  test with the following from the assignment:
  withVars [("x", 6)] $ add (lit 3) (var "x") - Just 9
  withVars [("x", 6)] $ add (lit 3) (var "y") = Nothing -- because "y" is not in the map
  withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x")) == Just 54
-}
