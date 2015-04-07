{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Control.Applicative
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
  deriving Show

-- yay pointless!
sortedDice :: Int -> Rand StdGen [DieValue]
sortedDice = (sortBy (flip compare) <$>) . flip replicateM die

resolveOne :: Battlefield -> (DieValue,DieValue) -> Battlefield
resolveOne b (ar, dr)
    | ar > dr   = b {defenders = defenders b - 1}
    | otherwise = b {attackers = attackers b - 1}

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  attackRolls <- sortedDice $ min 3 (attackers b - 1)
  defendRolls <- sortedDice $ min 2 (defenders b)
  return $ foldl resolveOne b $ zip attackRolls defendRolls

invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
    newB <- battle b
    if attackers newB < 2 || defenders newB <= 0
    then return newB
    else invade newB

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  simulations <- replicateM 1000 (invade b)
  let wins = length $ filter (0 ==) $ map defenders simulations
  return $ (fromIntegral wins) / 1000.0

-- simple main for running things
main:: IO()
main = do
  sp <- evalRandIO $ successProb (Battlefield 5 5)
  print sp
