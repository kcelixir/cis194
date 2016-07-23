{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Risk where

import Data.List
import Control.Monad
import Control.Monad.Random

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

type Army = Int
data Battlefield = Battlefield { attackers :: Army,
                                 defenders :: Army }
   deriving Show

instance Random DieValue where
  random            = first DV . randomR (1,6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

----------
-- Ex 2 --
----------
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield attackers defenders) = do
   ad <- dice $ min 3 $ attackers - 1
   dd <- dice $ min 2 $ defenders
   let raids = zip (reverse $ sort ad) (reverse $ sort dd)
   let routs = filter (uncurry (>)) raids
   return $ Battlefield (attackers - length raids + length routs)
                        (defenders - length routs)

----------
-- Ex 3 --
----------
invade :: Battlefield -> Rand StdGen Battlefield
invade field@(Battlefield attackers defenders) = do
   if attackers > 1 && defenders > 0 then
      battle field >>= invade
   else
      return field

----------
-- Ex 4 --
----------
successProb :: Battlefield -> Rand StdGen Double
successProb field = do
   invasions <- replicateM 1000 $ invade field
   let conquests = length $ filter (==0) $ map defenders invasions
   return $ (fromIntegral conquests) / 1000

----------
-- Ex 5 --
----------
exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield a d)
   | d < 1     = 1
   | a < 2     = 0
   | otherwise = (p0 + p1 + p2) / 3
                 where p0 = exactSuccessProb $ Battlefield a (d - 2)
                       p1 = exactSuccessProb $ Battlefield (a - 1) (d - 1)
                       p2 = exactSuccessProb $ Battlefield (a - 2) d
