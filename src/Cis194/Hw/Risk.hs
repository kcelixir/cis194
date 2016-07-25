{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Hw.Risk where

import           Control.Monad
import           Control.Monad.Random
import           Data.List

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

numAttackers :: Army -> Int
numAttackers a = min (a - 1) 3 - 1

numDefenders :: Army -> Int
numDefenders d = min (d - 1) 2

survivors :: (Monad m) => m [a] -> (a -> Bool) -> m Int
survivors l p = (length . filter p) <$> l

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  let att = sort <$> replicateM (numAttackers a) die
  let def = sort <$> replicateM (numDefenders d) die
  let winners = zipWith compare <$> att <*> def
  Battlefield <$> survivors winners (== GT) <*> survivors winners (/= GT)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield _ 0) = return bf
invade bf@(Battlefield 1 _) = return bf
invade bf = battle bf >>= invade

success :: Battlefield -> Bool
success b
  | defenders b == 0 = True
  | otherwise = False

attempt :: Int -> (Battlefield, Int) -> Rand StdGen (Battlefield, Int)
attempt 1000 (bf, s) = return (bf, s)
attempt n (bf, s) = invade bf >>= \r -> attempt (n + 1) (bf, s2 r)
  where
    s2 result
      | success result = s + 1
      | otherwise = s

successProb :: Battlefield -> Rand StdGen Double
successProb bf = attempt 0 (bf, 0) >>= \(_, s) -> return $ fromIntegral s/1000

-- instance Show (Rand StdGen Double) where
--  show d = d >>= print
