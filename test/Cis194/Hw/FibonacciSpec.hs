module Cis194.Hw.FibonacciSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.Fibonacci

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "should compute the nth Fibonacci number" $ do
      fib 0 `shouldBe` 0
      fib 1 `shouldBe` 1
      fib 2 `shouldBe` 1
      fib 14 `shouldBe` 377

  describe "fibs1" $ do
    it "should define an infinite list of all Fibonacci numbers" $ do
      take 7 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8]

  describe "fibs2" $ do
    it "should define an infinite list of all Fibonacci numbers" $ do
      take 7 fibs2 `shouldBe` [0, 1, 1, 2, 3, 5, 8]

  describe "streamRepeat" $ do
    it "should define a Stream of 5s" $ do
      (show $ streamRepeat 5) `shouldBe` (show $ take 20 $ repeat 5)

  describe "streamMap" $ do
    it "should define a Stream of 7s" $ do
      (show $ streamMap (+2) $ streamRepeat 5)
       `shouldBe` (show $ take 20 $ repeat 7)

  describe "streamFromSeed" $ do
    it "should define a Stream of sequential numbers" $ do
      (show $ streamFromSeed (+1) 1) `shouldBe` (show $ [1..20])

  describe "nats" $ do
    it "should define a Stream of sequential numbers" $ do
      (show $ nats) `shouldBe` (show $ [0..19])

  describe "interleaveStreams" $ do
    it "should define a Stream like [0,1,0,2,0,3..]" $ do
      (take 6 . streamToList $
       interleaveStreams (streamRepeat 0) (streamFromSeed (+1) 1))
        `shouldBe` [0,1,0,2,0,3]
        
  describe "ruler" $ do
    it "should define a Stream of the ruler function" $ do
      (take 16 $ streamToList ruler)
       `shouldBe` [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]
