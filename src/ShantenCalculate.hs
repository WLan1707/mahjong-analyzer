{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module ShantenCalculate where

import Hand
import AgariCheck
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.MemoTrie 
import Data.Maybe

shantenValue :: Int -> Int -> Int -> Int
shantenValue m p pm = 8 - 2*m - p - pm

type ResultSuit = (Int, Int, Int) -- (Meld, Pair, PMeld)
compResult :: ResultSuit -> Int
compResult (m, p, pm) = 2*m + p + pm

addResult :: ResultSuit -> ResultSuit -> ResultSuit
addResult (m1, p1, pm1) (m2, p2, pm2) = (m1+m2, p1+p2, pm1+pm2)

extractSuit ::  Suit -> HandCount -> Int
extractSuit suit handCount = encodeSuit [Map.findWithDefault 0 (Tile suit n) handCount | n <- [1..tileCount suit]]
  where 
    tileCount Honor = 7
    tileCount _     = 9
    encodeSuit counts = sum [c * (5 ^ i) | (i,c) <- zip [0..] counts]

decodeSuitMap :: Suit -> Int -> HandCount
decodeSuitMap suit num = Map.fromList [ (Tile suit n, c) | (n, c) <- zip [1..] ( decodeSuit num ), c > 0]
  where
    decodeSuit n = take 9 (step n)
      where
        step 0 = 0: step 0
        step x = (x `mod` 5) : step (x `div` 5)

calculateShanten :: HandCount -> Int
calculateShanten hand
  | isAgari hand = -1
  | otherwise    = uncurry3 shantenValue result
  where
    result = foldl addResult (0,0,0) resultPerSuit

    resultPerSuit =
      map perSuit [Manzu, Pinzu, Souzu, Honor]

    perSuit s =
      let sc = decodeSuitMap s (extractSuit s hand)
      in shantenSuitTileFirst sc (0,0,0)


shantenSuitTileFirst :: HandCount -> ResultSuit -> (Int, Int, Int)
shantenSuitTileFirst hand result
  | Map.null hand = result
  | otherwise =
      let t = fst (Map.findMin hand)

          cases =
            [ (isPair,     removePair    (Pair t),     (0,1,0))
            , (isTriplet,  removeTriplet (Triplet t),  (1,0,0))
            , (isSequence, removeSequence(Sequence t), (1,0,0))
            , (isMissMid,  removeMissMid (MissMid t),  (0,0,1))
            , (isMissOut,  removeMissOut (MissOut t),  (0,0,1)) ]

          stepResults =
            [ shantenSuitTileFirst (rm hand) (addResult result dl)
            | (ok, rm, dl) <- cases
            , ok t hand ]

          fallback = shantenSuitTileFirst (removeOneTile t hand) result

      in maximumBy (comparing compResult) (fallback : stepResults)

shantenInt
  :: (Int -> Suit -> ResultSuit -> ResultSuit)
  -> Int -> Suit -> ResultSuit -> ResultSuit
shantenInt f handInt suit result
  | handInt == 0 = result
  | otherwise =
      let
        hand = decodeSuitMap suit handInt
        t    = fst (Map.findMin hand)

        -- daftar kasus: predicate, remover, delta
        cases =
          [ (isPair,     removePair    (Pair t),     (0,1,0))
          , (isTriplet,  removeTriplet (Triplet t),  (1,0,0))
          , (isSequence, removeSequence(Sequence t), (1,0,0))
          , (isMissMid,  removeMissMid (MissMid t),  (0,0,1))
          , (isMissOut,  removeMissOut (MissOut t),  (0,0,1))
          ]

        -- hasil setiap langkah yang memungkinkan
        stepResults =
          [ f ( extractSuit suit (rm hand) ) suit (addResult result dl)
          | (ok, rm, dl) <- cases, ok t hand ]

        -- fallback jika tile bukan bagian dari blok
        fallback =
          f ( extractSuit suit (removeOneTile t hand) ) suit result

      in maximumBy (comparing compResult) (fallback : stepResults)


data Tree a = Tree (Tree a) a (Tree a)
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q

nats :: Tree Int
nats = go 0 1
    where
        go !n !s = Tree (go l s') n (go r s')
            where
                l = n + s
                r = l + s
                s' = s * 2

toList :: Tree a -> [a]
toList as = map (index as) [0..]

memoTree :: ((Int -> a) -> Int -> a) -> Int -> a
memoTree f = memoTree_f
  where memoTree_f = index memo
        memo = fmap (f memoTree_f) nats

memoShantenSuit :: Int -> Suit -> ResultSuit -> ResultSuit
memoShantenSuit = memoTree shantenInt

memoShanten :: HandCount -> Int
memoShanten hand
  | isAgari hand    = -1
  | otherwise       = uncurry3 shantenValue result
    where
      result = foldl addResult (0,0,0) resultPerSuit
      resultPerSuit = map helper suits
      suits = [Manzu, Pinzu, Souzu, Honor]
      helper suit = memoShantenSuit ( extractSuit suit hand ) suit (0,0,0)

-- test
handString = "123m235p122334s12z"
handString' = "1258m99p479s2237z"

testShantenMemo :: String -> IO ()
testShantenMemo str =
  case parseHand str of
    Left err   -> putStrLn $ "Parse error: " ++ err
    Right hand -> print $ memoShanten (handToCount hand)

testShanten :: String -> IO ()
testShanten str =
  case parseHand str of
    Left err   -> putStrLn $ "Parse error: " ++ err
    Right hand -> print $ calculateShanten (handToCount hand)
