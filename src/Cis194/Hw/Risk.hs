{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Hw.Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk
-- Ex2 --
type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

-- Rolls n dice
dice ::  Army -> Rand StdGen [DieValue]
dice n = sequence $ replicate n die

-- Returns result of single pair of dice
attack :: Battlefield -> (DieValue,DieValue) -> Battlefield
attack b (dieA, dieD)
  | dieA > dieD = b {defenders = defenders b - 1}
  | otherwise   = b {attackers = attackers b - 1}

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  diceA <- dice $ min 3 (attackers b - 1)
  diceD <- dice $ min 2 (defenders b)
  return $ foldl attack b $ zip (sort diceA) (sort diceD)

-- Ex3 --
invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | a < 2 || d == 0 = return b
  | otherwise       = battle b >>= invade

-- Ex4 --
successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  invasions <- sequence $ replicate 10000 $ invade b
  return $ foldl f 0.0 invasions where
  f p r = if defenders r == 0 then p + 0.0001 else p

main :: IO ()
main = do
  res <- evalRandIO $ successProb $ Battlefield 12 12
  putStrLn (show res)
