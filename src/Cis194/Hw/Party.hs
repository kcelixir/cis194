{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cis194.Hw.Party where

import Cis194.Hw.Employee
import Data.Monoid
import Data.Tree
import Data.List

-- Ex1 --
glCons :: Employee -> GuestList -> GuestList
glCons e (GL emps fun) = GL (e:emps) (fun + (empFun e))

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 > f2 then g1 else g2

-- Exercise 2 --
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) [treeFold f x | x <- (subForest t)]

-- Exercise 3 --
nextLevel :: Employee -> [(GuestList, GuestList)]
              -> (GuestList, GuestList)
nextLevel b [] = (glCons b mempty, mempty)
nextLevel b gls = foldl1 f gls
  where f (wid1,widout1) (wid2,widout2) = (
          glCons b $ mconcat [widout1, widout2],
          mconcat [(moreFun wid1 widout1), (moreFun wid2 widout2)])

-- Exercise 4 --
maxFun :: Tree Employee -> GuestList
maxFun company = uncurry moreFun $ treeFold nextLevel company

-- Exercise 5 --

guestList :: Tree Employee -> String
guestList company = unlines (totalFun funList : sortedEmpNames funList)
    where funList = maxFun company
          sortedEmpNames (GL emps _) = sort [empName e | e <- emps]
          totalFun (GL _ fun) = "Total fun: " ++ show fun

-- main :: IO ()
-- main = readFile "data/company.txt" >>= putStr . guestList . read
{-
Main is commented out so that tests can run.
Un-comment main and run this file using runhaskell -isrc

23:32 0 MBPSSpalding% runhaskell -isrc src/Cis194/Hw/Party.hs
Total fun: 41177
Ada Fitzgerald
Adam Bohrmann
Adam Eschmann
Adelaide Ahrens
...
-}
