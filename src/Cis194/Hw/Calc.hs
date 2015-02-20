module Cis194.Hw.Calc where 

import  Cis194.Hw.ExprT
import  Cis194.Hw.Parser

-- Exercise 1 --

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)

-- Exercise 2 --

evalStr :: String -> Maybe Integer
evalStr s = parseExp id (+) (*) s

-- Exercise 3 --

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit x = (Lit x)
  mul x y = (Mul x y)
  add x y = (Add x y)

-- Exercise 4 --

instance Expr Integer where
  lit x = x
  mul x y = (x*y)
  add x y = (x+y)
  
instance Expr Bool where
  lit x = (x>0)
  mul x y = (x && y)
  add x y = (x || y)
  
instance Expr MinMax where
  lit x = (MinMax x)
  mul (MinMax x) (MinMax y) = (MinMax (min x y))
  add (MinMax x) (MinMax y) = (MinMax (max x y))
  
instance Expr Mod7 where
  lit x = (Mod7 (mod x 7))
  mul (Mod7 x) (Mod7 y) = (Mod7 (mod (x*y) 7))
  add (Mod7 x) (Mod7 y) = (Mod7 (mod (x+y) 7))
  
newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

-- Exercise 5 --
