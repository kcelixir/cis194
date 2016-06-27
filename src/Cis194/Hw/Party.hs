{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Tree
import Data.List

import Employee

----------
-- Ex 1 --
----------
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

glFun :: GuestList -> Fun
glFun (GL _ f) = f

instance Monoid GuestList where
   mempty               = GL [] 0
   mappend (GL es _) gl = foldr glCons gl es

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

----------
-- Ex 2 --
----------
treeFold :: (Monoid b) => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a []) = f a mempty
treeFold f (Node a cs) = f a $ map (treeFold f) cs

----------
-- Ex 3 --
----------
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls = (mconcat $ glCons e mempty : map snd gls,
                   mconcat $ map fst gls)

----------
-- Ex 4 --
----------
maxFun :: Tree Employee -> GuestList
maxFun t = moreFun wi wo
           where (wi, wo) = treeFold nextLevel t

----------
-- Ex 5 --
----------
companyNames :: Employee -> [[String]] -> [String]
companyNames e ls = foldr (++) [(empName e)] ls

evalCompany :: String -> (Fun, [String])
evalCompany str = (glFun $ maxFun tree, sort names)
                  where tree  = read str :: Tree Employee
                        names = treeFold companyNames tree

main :: IO ()
main = do
   file <- readFile "../../../data/company.txt"
   let (fun, names) = evalCompany file
   putStrLn $ "Total fun: " ++ show fun
   mapM_ putStrLn names
