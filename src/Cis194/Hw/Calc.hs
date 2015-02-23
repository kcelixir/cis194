module Cis194.Hw.Calc where 

import Cis194.Hw.ExprT
import Cis194.Hw.Parser

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
