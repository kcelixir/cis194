:set -i../../
:set prompt "> "
:l AParser.hs


(first (+1)) <$> (Just (1, 2))

runParser abParser "ab"
runParser abParser_ "abcdef"

runParser intPair "1234 324"

runParser intOrUppercase "234"
