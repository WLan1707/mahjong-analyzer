module Main where

import Test.QuickCheck
import Properties

main :: IO ()
main = do
  quickCheck prop_roundTrip
  quickCheck prop_partitionCorrect
  quickCheck prop_fuMultipleOf10
  quickCheck prop_fuAtLeast20
  quickCheck prop_scoreYakuman
  quickCheck prop_scorePermutationInvariant
