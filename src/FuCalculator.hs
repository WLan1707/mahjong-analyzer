{-# LANGUAGE NamedFieldPuns #-}

module FuCalculator where

import Hand
import AgariCheck
import Data.Tuple.Extra
import ShantenCalculate
import qualified Data.Map.Strict as Map
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.MemoTrie 
import YakuCalculator

isInPair tile (Pair p) = tile == p

isInKTriplet tile (KMeld (Triplet t) _) = tile == t
isInKTriplet tile _ = False

isInKSequence (Tile suit num) (KMeld (Sequence (Tile s n)) _) =
    (suit == s) && ( (num == n) || (num == n+1) || (num == n+2) )
isInKSequence tile _ = False

tilesInMeld (Sequence tile) = 
    [ Tile s n | n <- [n1..(n1+2)]]
    where 
        s = suitTile tile
        n1 = numberTile tile

tilesInMeld (Triplet tile) = [tile, tile, tile]

--------------------------------------------------------------
-- Wait Fu
--------------------------------------------------------------

-- pair wait
isTankiWait ctx (Standard _ pair) = isInPair (winTile ctx) pair
isTankiWait _ _ = False

-- isRyanmenWait ctx (Standard kmelds _) 
--     | s == Honor   = False
--     | otherwise     = (KMeld (Sequence wt) False `elem` kmelds && n /= 7) || (KMeld (Sequence (Tile s (n-2))) False `elem` kmelds && n /= 3)
--     where
--         wt = winTile ctx
--         s = suitTile wt
--         n = numberTile wt
-- isRyanmenWait _ _ = False
-- Ada di YakuCalculator.hs

isPenchanWait ctx (Standard kmelds _) =
    (KMeld (Sequence wt) False `elem` kmelds && n == 7) || (KMeld (Sequence (Tile s (n - 2))) False `elem` kmelds && n == 3)
    where
        wt = winTile ctx
        s = suitTile wt
        n = numberTile wt
isPenchanWait _ _ = False

isKanchanWait ctx (Standard kmelds _) =
    KMeld (Sequence (Tile s (n - 1))) False `elem` kmelds && n /= 9
    where 
        wt = winTile ctx
        s = suitTile wt
        n = numberTile wt
isKanchanWait _ _ = False 

isShanponWait ctx hand = not . or $ uncurry <$> [isTankiWait, isRyanmenWait, isPenchanWait, isKanchanWait] <*> [(ctx, hand)]

waitFu ctx hand =
    2 * fromEnum ( or $ uncurry <$> [isTankiWait, isPenchanWait, isKanchanWait] <*> [(ctx, hand)])

----------------------------------------------------------------------
-- Meld Fu
----------------------------------------------------------------------

meldFu (KMeld (Sequence _) _) = 0
meldFu (KMeld (Triplet t) isOpen) =
    let 
        openFactor      = 1 + fromEnum (not isOpen)
        simpleFactor    = 1 + fromEnum (not $ isSimpleTile t)
    
    in 2 * openFactor * simpleFactor

pairFu ctx (Pair p) = 
    2 * (fromEnum (isDragon p) + fromEnum (n == pw) + fromEnum (n == sw))
    where
        pw = prevalentWind ctx
        sw = seatWind ctx
        n = numberTile p

---------------------------------------------------------------------------
-- Main Fu Calculation
---------------------------------------------------------------------------

roundFu fu = max 20 (((fu + 9) `div` 10) * 10)

calcFu _ (Chiitoi _) = 25
calcFu _ (Kokushi _) = 0
calcFu ctx hand@(Standard kmelds pair) =

    let
        -- if pinfu and tsumo -> total fu is 20
        isTsumoWin = case winMethod ctx of {Tsumo -> True; _ -> False }
        hasPinfu = pinfu ctx hand > 0
        
        specialPinfuTsumo = hasPinfu && isTsumoWin

        -- base 20 fu for normal hand
        base = 20
        
        -- sum of meld fu
        mf = sum $ map meldFu kmelds

        -- pair fu
        pf = pairFu ctx pair

        -- wait fu
        wf = waitFu ctx hand

        -- tsumo fu: +2 for tsumo without pinfu
        tsumoBonus = 2 * fromEnum (isTsumoWin && not hasPinfu)

        -- menzen ron bonus: +10 if ron with menzenchin
        menzenRonBonus = 10 * fromEnum ( not isTsumoWin && null (openMelds ctx) )

        -- total before rounding:
        totalRaw = 
            if specialPinfuTsumo then 20
            else base + mf + pf + wf + tsumoBonus + menzenRonBonus

        total = roundFu totalRaw

    in total