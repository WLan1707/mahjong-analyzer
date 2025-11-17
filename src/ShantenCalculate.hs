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
  | isAgari hand    = -1
  | otherwise       = uncurry3 shantenValue result
    where
      result = foldl addResult (0,0,0) resultPerSuit
      resultPerSuit = [resultMan, resultPin, resultSou, resultHon]
      resultMan = shantenSuitTileFirst (decodeSuitMap Manzu $ extractSuit Manzu hand ) (0,0,0)
      resultPin = shantenSuitTileFirst (decodeSuitMap Pinzu $ extractSuit Pinzu hand ) (0,0,0)
      resultSou = shantenSuitTileFirst (decodeSuitMap Souzu $ extractSuit Souzu hand ) (0,0,0)
      resultHon = shantenSuitTileFirst (decodeSuitMap Honor $ extractSuit Honor hand ) (0,0,0)

shantenSuitTileFirst :: HandCount -> ResultSuit -> ResultSuit
shantenSuitTileFirst hand result
  | Map.null hand     = result
  | otherwise =
    let
      smallestTile = fst $ Map.findMin hand
      
      pairResult = [ shantenSuitTileFirst (removePair (Pair smallestTile) hand) (addResult result (0,1,0)) | isPair smallestTile hand ]
      tripletResult = [ shantenSuitTileFirst (removeTriplet (Triplet smallestTile) hand) (addResult result (1,0,0)) | isTriplet smallestTile hand ]
      sequenceResult = [ shantenSuitTileFirst (removeSequence (Sequence smallestTile) hand) (addResult result (1,0,0)) | isSequence smallestTile hand ]
      missMidResult = [ shantenSuitTileFirst (removeMissMid (MissMid smallestTile) hand) (addResult result (0,0,1)) | isMissMid smallestTile hand ]
      missOutResult = [ shantenSuitTileFirst (removeMissOut (MissOut smallestTile) hand) (addResult result (0,0,1)) | isMissOut smallestTile hand ]
      fallbackResult = [ shantenSuitTileFirst (removeOneTile smallestTile hand) result ]
      allResult = pairResult ++ tripletResult ++ sequenceResult ++ missMidResult ++ missOutResult ++ fallbackResult
    in maximumBy (comparing compResult) allResult

shantenInt :: (Int -> Suit -> ResultSuit -> ResultSuit) -> Int -> Suit -> ResultSuit -> ResultSuit
shantenInt f handInt suit result
  | handInt == 0     = result
  | otherwise =
    let
      hand = decodeSuitMap suit handInt
      smallestTile = fst $ Map.findMin hand
      
      pairResult = [ f (extractSuit suit $ removePair (Pair smallestTile) hand) suit (addResult result (0,1,0)) | isPair smallestTile hand ]
      tripletResult = [ f (extractSuit suit $ removeTriplet (Triplet smallestTile) hand) suit (addResult result (1,0,0)) | isTriplet smallestTile hand ]
      sequenceResult = [ f (extractSuit suit $ removeSequence (Sequence smallestTile) hand) suit (addResult result (1,0,0)) | isSequence smallestTile hand ]
      missMidResult = [ f (extractSuit suit $ removeMissMid (MissMid smallestTile) hand) suit (addResult result (0,0,1)) | isMissMid smallestTile hand ]
      missOutResult = [ f (extractSuit suit $ removeMissOut (MissOut smallestTile) hand) suit (addResult result (0,0,1)) | isMissOut smallestTile hand ]
      fallbackResult = [ f (extractSuit suit $ removeOneTile smallestTile hand) suit result ]
      
      allResult = pairResult ++ tripletResult ++ sequenceResult ++ missMidResult ++ missOutResult ++ fallbackResult
    in maximumBy (comparing compResult) allResult

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
      resultPerSuit = [resultMan, resultPin, resultSou, resultHon]
      resultMan = memoShantenSuit ( extractSuit Manzu hand ) Manzu (0,0,0)
      resultPin = memoShantenSuit ( extractSuit Pinzu hand ) Pinzu (0,0,0)
      resultSou = memoShantenSuit ( extractSuit Souzu hand ) Souzu (0,0,0)
      resultHon = memoShantenSuit ( extractSuit Honor hand ) Honor (0,0,0)

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
