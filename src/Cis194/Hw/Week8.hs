{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cis194.Hw.Week8 where
import           Cis194.Hw.Employee
import           Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL guests fun) = GL (e : guests) (fun + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL g1 f1) (GL g2 f2) = GL (g1 ++ g2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Ex. 2
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold v f (Node e []) = f e [v]
treeFold v f (Node e xs) = f e $ map (treeFold v f) xs

-- Ex. 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (GL [e] $ empFun e, mempty)
nextLevel e gls = (glCons e $ snd l, fst l) where
  l = foldr1 mappend gls

-- Ex. 4
maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun ls where
  ls = treeFold (mempty, mempty) nextLevel t

-- Ex. 5
main :: IO ()
main = do
  company <- readFile "../data/company.txt"
  let (GL gs f) = maxFun $ read company
  putStr "Total fun: "
  print f
  putStrLn ""
  mapM_ (putStrLn . empName) gs
