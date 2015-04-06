{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
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

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  aRolls <- dice (maxA a)
  dRolls <- dice (maxD d)
  let outcomes = zipWith (>) aRolls dRolls
  let newA = survivors a outcomes False
  let newD = survivors d outcomes True
  return $ Battlefield newA newD
  where
    maxA = max 0 . min 3 . subtract 1
    maxD = max 0 . min 2
    survivors orig outcomes side = orig - (length . filter (==side) $ outcomes)

dice x = do
  rolls <- replicateM x die
  return $ reverse . sort $ map unDV rolls

-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | (a < 2 || d <= 0) = return b
  | otherwise = (battle b) >>= invade

-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  results <- replicateM 1000 (invade b)
  let wins = length $ filter (>2) $ map attackers results
  return $ (fromIntegral wins) / 1000.0
