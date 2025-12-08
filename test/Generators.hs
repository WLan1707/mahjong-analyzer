-- {-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Generators where

import Test.QuickCheck
import Hand
-- import AgariCheck
import Data.List (nub)
-- import Data.Map.Strict (toList)

------------------------------------------------------------
-- Tile generator
------------------------------------------------------------

instance Arbitrary Suit where
  arbitrary = elements [Manzu, Pinzu, Souzu, Honor]

genTile :: Gen Tile
genTile = do
  suit <- arbitrary
  n <- case suit of
    Honor -> choose (1,7)
    _ -> choose (1,9)
  return (Tile suit n)

instance Arbitrary Tile where
  arbitrary = genTile

------------------------------------------------------------
-- Meld & KMeld
------------------------------------------------------------

genSequence :: Gen Meld
genSequence = do
  suit <- elements [Manzu, Pinzu, Souzu]
  n    <- choose (1,7)
  return (Sequence (Tile suit n))

genTriplet :: Gen Meld
genTriplet = Triplet <$> genTile

instance Arbitrary Meld where
  arbitrary = oneof [genSequence, genTriplet]

instance Arbitrary KMeld where
  arbitrary = KMeld <$> arbitrary <*> arbitrary

------------------------------------------------------------
-- Pair
------------------------------------------------------------

instance Arbitrary Pair where
  arbitrary = Pair <$> genTile

------------------------------------------------------------
-- AgariHand
------------------------------------------------------------

genStandard :: Gen AgariHand
genStandard = Standard <$> vectorOf 4 arbitrary <*> arbitrary

genChiitoi :: Gen AgariHand
genChiitoi = do
  ts <- nub <$> vectorOf 7 arbitrary
  return (Chiitoi (map Pair ts))

genKokushi :: Gen AgariHand
genKokushi = do
  terminals <- shuffle kokushiList
  let pair = head terminals
  return (Kokushi (pair:terminals))

kokushiList :: [Tile]
kokushiList =
  [ Tile s n | s <- [Manzu, Pinzu, Souzu], n <- [1,9] ] ++
  [ Tile Honor n | n <- [1..7] ]

instance Arbitrary AgariHand where
  arbitrary = frequency
    [ (8, genStandard)
    , (1, genChiitoi)
    , (1, genKokushi)
    ]

------------------------------------------------------------
-- HandContext
------------------------------------------------------------

instance Arbitrary WinMethod where
  arbitrary = elements [Ron, Tsumo]

instance Arbitrary HandContext where
  arbitrary = do
    wt <- arbitrary
    wm <- arbitrary
    dealer <- arbitrary
    pw <- choose (1,4)
    sw <- choose (1,4)
    r <- arbitrary
    let openM = []  -- no open melds in default tests
    return HandContext
      { winTile = wt
      , winMethod = wm
      , isDealer = dealer
      , prevalentWind = pw
      , seatWind = sw
      , openMelds = openM
      , isRiichi = r
      }

------------------------------------------------------------
-- Convert AgariHand → Hand
------------------------------------------------------------

agariToHand :: AgariHand -> Hand
agariToHand (Chiitoi ps) = concatMap (\(Pair t) -> [t,t]) ps
agariToHand (Kokushi ts) = ts
agariToHand (Standard km pair) =
  concatMap kmeldToTiles km ++ pairToTiles pair

kmeldToTiles :: KMeld -> [Tile]
kmeldToTiles (KMeld m _) = meldToTiles m

meldToTiles :: Meld -> [Tile]
meldToTiles (Sequence t) = [t, t { numberTile = numberTile t + 1}, t { numberTile = numberTile t + 2}]
meldToTiles (Triplet t) = [t,t,t]

pairToTiles :: Pair -> [Tile]
pairToTiles (Pair t) = [t,t]
