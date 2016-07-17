:set -i../../
:set prompt "> "
:l SExpr.hs

runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
runParser (spaces *> posInt) " 345"

runParser sexpr " 1 "
runParser sexpr " test"
runParser sexpr "( filter pos (map inc (2 3 4 )) )"
