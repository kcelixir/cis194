:set -i../../
:set prompt "> "
:l Risk.hs

evalRandIO (invade $ Battlefield 2 1)
evalRandIO (successProb $ Battlefield 2 1)
exactSuccessProb $ Battlefield 3 1
