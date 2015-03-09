module Cis194.Hw.JoinListSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.JoinList
import Cis194.Hw.Sized
import Control.Applicative
import Data.Monoid

main :: IO ()
main = hspec spec


(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jl = (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b'))

spec :: Spec
spec = do
  describe "Append" $ do
    it "Should append two JoinLists" $ do
      ((Single (Product 2) 'a')+++(Single (Product 3) 'b'))
      `shouldBe`
      (Append (Product 6) (Single (Product 2) 'a') (Single (Product 3) 'b'))

  describe "indexJ" $ do
    it "Should return the item at index 0" $ do
      (indexJ 0 jl) `shouldBe` (jlToList jl !!? 0)
    it "Should return the item at index 1" $ do
      (indexJ 1 jl) `shouldBe` (jlToList jl !!? 1)

  describe "dropJ" $ do
    it "Should drop 0" $ do
      jlToList (dropJ 0 jl) `shouldBe` drop 0 (jlToList jl)
    it "Should drop 1" $ do
      jlToList (dropJ 1 jl) `shouldBe` drop 1 (jlToList jl)
    it "Should drop 2" $ do
      jlToList (dropJ 2 jl) `shouldBe` drop 2 (jlToList jl)

  describe "takeJ" $ do
    it "Should drop 0" $ do
      jlToList (takeJ 0 jl) `shouldBe` take 0 (jlToList jl)
    it "Should drop 1" $ do
      jlToList (takeJ 1 jl) `shouldBe` take 1 (jlToList jl)
    it "Should drop 2" $ do
      jlToList (takeJ 2 jl) `shouldBe` take 2 (jlToList jl)
