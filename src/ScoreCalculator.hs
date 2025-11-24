-- {-# LANGUAGE NamedFieldPuns #-}

module ScoreCalculator where

import Hand
import AgariCheck
import Data.Tuple.Extra
import ShantenCalculate
import qualified Data.Map.Strict as Map
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.MemoTrie 
import YakuCalculator
import FuCalculator

scoreFinal :: Bool -> Int -> Int -> Int
scoreFinal isDealer' han fu
    | han >= 13 = 8000 * dealerMult -- yakuman
    | han >= 11 = 6000 * dealerMult -- sanbaiman
    | han >= 8  = 4000 * dealerMult -- baiman
    | han >= 6  = 3000 * dealerMult -- haneman
    | han == 5  = 2000 * dealerMult
    | base >= 2000 = 2000 * dealerMult
    | otherwise = roundUp100 (base * dealerMult)
  where
    base = fu * (2 ^ (han + 2))

    dealerMult
      | isDealer'  = 6
      | otherwise = 4

    roundUp100 x = ((x + 99) `div` 100) * 100

calcScore :: HandContext -> AgariHand -> Int
calcScore ctx ag =
    let fu  = calcFu  ctx ag
        han = calcHan ctx ag
        isDealer' = isDealer ctx
        wt = winMethod ctx
    in scoreFinal isDealer' han fu

finalScore :: HandContext -> HandCount -> Int
finalScore ctx hc =
    let agaris = findPartition hc (openMelds ctx)
    in if null agaris
          then 0
          else maximum (0 : [ calcScore ctx ag | ag <- agaris ])


--------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------

testScore :: HandContext -> String -> Either String Int
testScore ctx str = do
    hand <- parseHand str
    let hc = handToCount hand
        ags = findPartition hc (openMelds ctx)
    if null ags
       then Left "Not a winning hand"
       else Right $ maximum [ calcScore ctx ag | ag <- ags ]

testYaku ctx yaku str = do
    hand <- parseHand str
    let hc = handToCount hand
        ags = findPartition hc (openMelds ctx)
    if null ags
      then Left "Not a winning hand"
      else Right $ [ yaku ctx ag | ag <- ags] 

ctxRon :: Tile -> HandContext
ctxRon wt = HandContext
  { winTile    = wt
  , winMethod  = Ron
  , isDealer   = False
  , seatWind = 1
  , prevalentWind  = 1
  , openMelds  = []
  , isRiichi   = False
  }

runTest :: String -> HandContext -> IO ()
runTest str ctx =
    case testScore ctx str of
      Left err   -> putStrLn ("Error: " ++ err)
      Right pts  -> putStrLn ("Score = " ++ show pts)



tes1 = runTest "111234m456p789s55z" (ctxRon (Tile Manzu 3))
-- expected: Score = 1300
tes2 = runTest "111222m333s444z55p" (ctxRon (Tile Souzu 3))
-- expected: Score = 8000
tes3 = runTest "112233m4455p66s77z" (ctxRon (Tile Honor 7))
-- expected: 1600
