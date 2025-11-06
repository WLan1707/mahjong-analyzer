module ShantenCalculate where

import Hand
import AgariCheck
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)

shantenValue :: Int -> Int -> Int -> Int
shantenValue m p pm = 8 - 2*m - p - pm

type ResultSuit = (Int, Int, Int) -- (Meld, Pair, PMeld)
compResult :: ResultSuit -> Int
compResult (m, p, pm) = 2*m + p + pm

addResult :: ResultSuit -> ResultSuit -> ResultSuit
addResult (m1, p1, pm1) (m2, p2, pm2) = (m1+m2, p1+p2, pm1+pm2)

extractSuit ::  Suit -> HandCount -> [Int]
extractSuit suit handCount = [Map.findWithDefault 0 (Tile suit n) handCount | n <- [1..tileCount suit]]
  where 
    tileCount Honor = 7
    tileCount _     = 9

encodeSuit :: [Int] -> Int
encodeSuit counts = sum [c * (5 ^ i) | (i,c) <- zip [0..] counts]

decodeSuit :: Int -> [Int]
decodeSuit n = take 9 (step n)
  where
    step 0 = 0: step 0
    step x = (x `mod` 5) : step (x `div` 5)

decodeSuitMap :: Suit -> [Int] -> HandCount
decodeSuitMap suit counts = Map.fromList [ (Tile suit n, c) | (n, c) <- zip [1..] counts, c > 0]

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

-- test
handString = "123m235p122334s12z"

testShanten :: String -> IO ()
testShanten str =
  case parseHand str of
    Left err   -> putStrLn $ "Parse error: " ++ err
    Right hand -> print $ calculateShanten (handToCount hand)
