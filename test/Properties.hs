module Properties where

import Test.QuickCheck
import Hand
import Generators
import AgariCheck
import YakuCalculator
import FuCalculator
import ScoreCalculator

import Data.List (sort)

------------------------------------------------------------
-- Property: HandCount round-trip
------------------------------------------------------------

prop_roundTrip :: AgariHand -> Bool
prop_roundTrip ag =
  let h  = fst (agariToHand ag)
      hc = handToCount h
  in sort (countToHand hc) == sort h

------------------------------------------------------------
-- Property: findPartition reconstructs hand
------------------------------------------------------------

prop_partitionCorrect :: AgariHand -> Bool
prop_partitionCorrect ag =
  let h  = fst (agariToHand ag)
      kms = snd (agariToHand ag)
      hc = handToCount h
      ag' = normalizeAgari ag
  in ag' `elem` map normalizeAgari (findPartition hc kms)

------------------------------------------------------------
-- Property: Fu properties
------------------------------------------------------------

prop_fuMultipleOf10 :: HandContext -> AgariHand -> Bool
prop_fuMultipleOf10 ctx ag =
  case ag of
    Chiitoi _ -> calcFu ctx ag == 25
    _         -> calcFu ctx ag `mod` 10 == 0

prop_fuAtLeast20 :: HandContext -> AgariHand -> Bool
prop_fuAtLeast20 ctx ag =
  let f = calcFu ctx ag
  in case ag of
        Kokushi _ -> f == 0
        Chiitoi _ -> f == 25
        _         -> f >= 20

------------------------------------------------------------
-- Property: Score limits
------------------------------------------------------------

prop_scoreMin :: HandContext -> AgariHand -> Property
prop_scoreMin ctx ag =
  winTile ctx `elem` fst (agariToHand ag) ==>
    let s = calcScore ctx ag
    in s >= 1000 || s == 0
  -- || calcHan ctx ag >= 13  -- yakuman exception


prop_scoreYakuman :: HandContext -> AgariHand -> Property
prop_scoreYakuman ctx ag =
  calcHan ctx ag >= 13 ==>
    let sc = calcScore ctx ag
    in sc == 32000 || sc == 48000

------------------------------------------------------------
-- Property: score invariant under hand permutation
------------------------------------------------------------

prop_scorePermutationInvariant :: HandContext -> AgariHand -> Property
prop_scorePermutationInvariant ctx ag =
  let h  = fst (agariToHand ag)
      kms = snd (agariToHand ag)
      sc = calcScore ctx ag
  in forAll (shuffle h) $ \h2 ->
       let hc2 = handToCount h2
           ag2s = findPartition hc2 kms
       in null ag2s || maximum (map (calcScore ctx) ag2s) == sc
