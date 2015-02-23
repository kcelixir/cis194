{-# LANGUAGE FlexibleInstances #-}
module Cis194.Hw.Calc where 

import  Cis194.Hw.ExprT
import  Cis194.Hw.Parser
import qualified Cis194.Hw.StackVM as Stk

-- Exercise 1 --

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)

-- Exercise 2 --

evalStr :: String -> Maybe Integer
evalStr = parseExp id (+) (*)

-- Exercise 3 --

class Expr a where
  lit :: Integer -> a
  mul, add :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add

-- Exercise 4 --

instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)
  
instance Expr Bool where
  lit = (>0)
  mul = (&&)
  add = (||)
  
instance Expr MinMax where
  lit = MinMax
  mul (MinMax x) (MinMax y) = MinMax $ min x y
  add (MinMax x) (MinMax y) = MinMax $ max x y
  
instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x*y) 7
  add (Mod7 x) (Mod7 y) = Mod7 $ mod (x+y) 7
  
newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

-- Exercise 5 --

instance Expr Stk.Program where
  lit x = [Stk.PushI x] 
  mul x y = x ++ y ++ [Stk.Mul]
  add x y = x ++ y ++ [Stk.Add]

compile :: String -> Maybe Stk.Program
compile = parseExp lit add mul

-- Test:
testProgram = compile "(7+3)*2"
runStack (Just p) = Stk.stackVM p
parseVal (Right (Stk.IVal x)) = x
testVal = parseVal $ runStack testProgram

{-
class ExprS a where
  slit :: Integer -> Stk.Progam
  smul, sadd :: a -> a -> a

instance ExprS ExprT where
  slit = Lit
  smul = Mul
  sadd = Add

instance ExprS Integer where
  slit = Stk.PushI
  smul = Stk.And
  sadd = Stk.Or

instance ExprS Bool where
  slit = [Stk.PushB]
  smul x y = Stk.And
  sadd = Stk.Or

seval :: ExprS -> Stk.Program
seval (Lit (Integer x)) = [Stk.PushI x]
seval (Lit (Bool x)) = [Stk.PushB x]
seval (Mul x y) = (seval x) ++ (seval y) ++ [Stk.Mul]
seval (Add x y) = (seval x) ++ (seval y) ++ [Stk.Add]
seval (And x y) = (seval x) ++ (seval y) ++ [Stk.And]
seval (Or  x y) = (seval x) ++ (seval y) ++ [Stk.Or]
seval _ = []

compile :: String -> Maybe Program
compile = (seval (parseExp slit sadd smul)) 
-}
