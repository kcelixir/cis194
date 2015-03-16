{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Monoid
import Data.Tree
import Data.List
import Employee

--  Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (fun + (empFun emp))

instance Monoid GuestList where
  mempty  = GL [] 0
  GL esa fa `mappend` GL esb fb = GL (esa ++ esb) (fa + fb)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

-- testCompany
--   = Node (Emp "Stan" 9)
--     [ Node (Emp "Bob" 2)
--       [ Node (Emp "Joe" 5)
--         [ Node (Emp "John" 1) []
--         , Node (Emp "Sue" 5) []
--         ]
--       , Node (Emp "Fred" 3) []
--       ]
--     , Node (Emp "Sarah" 17)
--       [ Node (Emp "Sam" 4) []
--       ]
--     ]
-- We have to use map here since emps is a list of trees
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node boss emps) = f boss $ map (treeFold f) emps

-- Exercise 3

-- bestWithBoss: In order to get the best list with the boss we simply take the second list
-- of each pair since that pair is
--
-- bestSansBoss: Without the boss then the result will be whichever pair is
-- greater concatenated together
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss pairs = (bestWithBoss, bestSansBoss)
  where bestWithBoss = glCons boss $ mconcat $ map snd pairs
        bestSansBoss = mconcat $ map (uncurry moreFun) pairs

-- Exercise 4
--
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
totals :: GuestList -> String
totals (GL emps fun) = total ++ names
  where
    total = "Total fun: " ++ (show fun) ++ "\n"
    names = concat $ map(++"\n") $ sort $ map (empName) emps

main :: IO ()
main = readFile "../../../data/company.txt" >>= putStrLn . totals . maxFun .read
