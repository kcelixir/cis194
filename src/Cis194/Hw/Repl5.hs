:set -i../../
:set prompt "> "
:l Calc.hs
:l ../../../test/Cis194/Hw/CalcSpec.hs

main

eval $ Mul (Add (Lit 2) (Lit 3)) (Lit 4)

evalStr "(2 + 3) * 4"

mul (add (lit 2) (lit 3)) (lit 4) :: Integer
add (lit 1) (lit (-1))  :: Bool
mul (add (lit 2) (lit 3)) (lit 4) :: MinMax
mul (add (lit 2) (lit 3)) (lit 4) :: Mod7

compile "(2 + 3) * 4"
