module Cis194.Hw.PartySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cis194.Hw.Party
import Cis194.Hw.Employee

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "glCons" $ do
    it "should cons Employee to a Guestlist" $ do
        glCons (Emp "Bob" 3) (GL [(Emp "Stan" 2)] 2)
          `shouldBe`
            GL [(Emp "Bob" 3), (Emp "Stan" 2)] 5

  describe "moreFun" $ do
    it "should return the more fun Employee" $ do
        moreFun (GL [(Emp "Bob" 3)] 3) (GL [(Emp "Stan" 2)] 2)
          `shouldBe` (GL [(Emp "Bob" 3)] 3)

  describe "maxFun" $ do
    it "should return the GuestList with the mostest fun" $ do
        maxFun testCompany
        `shouldBe`
        (GL [(Emp "Stan" 9),(Emp "John" 1),(Emp "Sue" 5),(Emp "Fred" 3)] 18)

    it "should return the GuestList with the mostest fun" $ do
        maxFun testCompany2
        `shouldBe`
        (GL [(Emp "Stan" 9),(Emp "John" 1),(Emp "Sue" 5),(Emp "Fred" 3)] 18)
