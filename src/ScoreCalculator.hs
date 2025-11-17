{-# LANGUAGE NamedFieldPuns #-}

module ScoreCalculator where

import Hand
import AgariCheck
import Data.Tuple.Extra
import ShantenCalculate
import qualified Data.Map.Strict as Map
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.MemoTrie 

data WinMethod = Ron | Tsumo 
    deriving (Show, Eq)

data HandContext = HandContext {
    openMelds :: [KMeld]
    , roundWind :: Int
    , playerWind :: Int
    , winTile :: Tile
    , winMethod :: WinMethod
    , isRiichi :: Bool
    , isDealer :: Bool
}

isMenzen :: HandContext -> Bool
isMenzen = null . openMelds

isOpenKMeld (KMeld _ b) = b

isSequenceMeld :: KMeld -> Bool
isSequenceMeld (KMeld (Sequence _) _) = True
isSequenceMeld _ = False

isTripletMeld :: KMeld -> Bool
isTripletMeld (KMeld (Triplet _) _) = True
isTripletMeld _ = False

isSimpleTile :: Tile -> Bool
isSimpleTile (Tile Honor _) = False
isSimpleTile (Tile _ 1)     = False
isSimpleTile (Tile _ 9)     = False
isSimpleTile _              = True

isSimplePair :: Pair -> Bool
isSimplePair (Pair tile) = isSimpleTile tile

isSimpleSequence :: Meld -> Bool
isSimpleSequence (Sequence (Tile s n)) =
    isSimpleTile (Tile s n) &&
    isSimpleTile (Tile s (n+1)) &&
    isSimpleTile (Tile s (n+2))
isSimpleSequence _ = False

isSimpleTriplet :: Meld -> Bool
isSimpleTriplet (Triplet t) =
    isSimpleTile t
isSimpleTriplet _ = False

allMeldSimple (Sequence tile) = isSimpleSequence (Sequence tile)
allMeldSimple (Triplet tile) = isSimpleTriplet (Triplet tile)

-----------------------------------------------------------------
-- List Yaku
-----------------------------------------------------------------

-- 1 Han Closed Only

menzenchin :: HandContext -> AgariHand -> Int
menzenchin ctx _ = fromEnum $ isMenzen ctx && (winMethod ctx == Tsumo)

riichi ctx _ = fromEnum $ isRiichi ctx

pinfu :: HandContext -> AgariHand -> Int
pinfu ctx (Standard kmelds pair)
    | not (isMenzen ctx)          = 0
    | (not . all isSequenceMeld) kmelds = 0
    | pairIsYakuhai pair ctx         = 0
    | not (isRyanmenWait ctx kmelds pair) = 0
    | otherwise                      = 1
pinfu _ _ = 0

pairIsYakuhai :: Pair -> HandContext -> Bool
pairIsYakuhai (Pair (Tile Honor n)) ctx =
       n == playerWind ctx
    || n == roundWind ctx
    || n >= 5   -- dragons
pairIsYakuhai _ _ = False

isRyanmenWait :: HandContext -> [KMeld] -> Pair -> Bool
isRyanmenWait ctx kmelds pair =
    case findSequenceContainingWinTile kmelds (winTile ctx) of
        Just (Sequence (Tile s n)) ->
            let w = winTile ctx in
            case w of
                Tile _ wN
                    -- (wN == n+2) berarti n,n+1,wN → menang kanan
                    -- (wN == n)   berarti wN,n+1,n+2 → menang kiri
                    | wN == n     -> True
                    | wN == n + 2 -> True
                    | otherwise   -> False
        Nothing -> False

findSequenceContainingWinTile :: [KMeld] -> Tile -> Maybe Meld
findSequenceContainingWinTile kmelds wt =
    case [ m | KMeld m@(Sequence t) _ <- kmelds, tileInSequence wt t ] of
        (m:_) -> Just m
        []    -> Nothing

tileInSequence :: Tile -> Tile -> Bool
tileInSequence (Tile s n) (Tile s2 n2)
    = s == s2 && n >= n2 && n <= n2 + 2


-- pure double sequence
iipeikou ctx (Standard kmelds pair) 
    | not $ isMenzen ctx = 0
    | length ( filter (\m -> isSequenceMeld m && freq m == 2)  (nub kmelds)) == 1 = 1
    | otherwise = 0
        where freq y = length $ filter (== y) kmelds
iipeikou _ _ = 0 -- Masih salah masalah persamaan KMeld yang ada isOpen nya, ryanpeikou juga.

-- 1 Han

-- all simple
tanyao :: HandContext -> AgariHand -> Int
tanyao _ (Standard kmelds pair)
    | all isSimpleTile pairTiles
      && all (allMeldSimple . baseMeld) kmelds
        = 1
    | otherwise = 0
  where
    pairTiles = case pair of
        Pair t -> [t, t]

tanyao _ _ = 0

-- Triplet Honor
yakuhai :: HandContext -> AgariHand -> Int
yakuhai ctx (Standard kmelds _) = length [ num | (KMeld (Triplet (Tile Honor num)) _) <- kmelds, honor <- [5,6,7,roundWind ctx, playerWind ctx], num == honor]
yakuhai _ _ = 0

-- 2 Han

-- Half-Outside
chantaiyao :: HandContext -> AgariHand -> Int
chantaiyao ctx (Standard kmelds pair) 
    | (not . any isSimpleTile) pairTiles
    && (not . any (allMeldSimple . baseMeld)) kmelds
    = 2 - fromEnum (isMenzen ctx)
    | otherwise = 0
    where 
        pairTiles = case pair of
            Pair t -> [t,t]


-- 
sanshoku :: HandContext -> AgariHand -> Int
sanshoku _ (Standard kmelds _) =
    if anyStartSanshoku kmelds
       then if (not . any isOpen) kmelds then 2 else 1
       else 0
sanshoku _ _ = 0


anyStartSanshoku :: [KMeld] -> Bool
anyStartSanshoku kmelds =
    let starts = 
          [ (suit,n)
          | KMeld (Sequence (Tile suit n)) _ <- kmelds
          , suit /= Honor
          ]
    in any (hasThreeSuits starts) [1..7]


hasThreeSuits :: [(Suit,Int)] -> Int -> Bool
hasThreeSuits starts n =
    all (`elem` starts)
      [ (Manzu, n)
      , (Pinzu, n)
      , (Souzu, n)
      ]

-- pure straight

ittsu :: HandContext -> AgariHand -> Int
ittsu ctx (Standard kmelds _) =
    let startsBySuit =
          [ (suit, n)
          | KMeld (Sequence (Tile suit n)) _ <- kmelds
          , suit /= Honor
          ]

        grouped =
          [ [ n | (s',n) <- startsBySuit, s' == s ]
          | s <- [Manzu, Pinzu, Souzu]
          ]

        ittsuFound =
          any (\ns -> all (`elem` ns) [1,4,7]) grouped

    in fromEnum ittsuFound * (fromEnum (isMenzen ctx) + 1)

ittsu _ _ = 0

-- all triplet
toitoi _ (Standard kmelds _) = 2 * fromEnum (all isTripletMeld kmelds)
toitoi _ _ = 0

-- seven pairs
chiitoitsu _ (Chiitoi pairs) = 2
chiitoitsu _ _ = 0

-- all terminal or honor
honroutou _ (Standard kmelds pair) = 2 * fromEnum (not (any (allMeldSimple . baseMeld) kmelds) && not ( isSimplePair pair ) && all isTripletMeld kmelds)
honroutou _ (Chiitoi pairs) = 2 * fromEnum (not (any isSimplePair pairs))
honroutou _ _ = 0

shousangen :: HandContext -> AgariHand -> Int
shousangen _ (Standard kmelds (Pair p)) = 2 * fromEnum (isDragon p && countDragonTriplets kmelds == 2)

isDragon :: Tile -> Bool
isDragon (Tile Honor n) = n `elem` [5,6,7]
isDragon _ = False

countDragonTriplets :: [KMeld] -> Int
countDragonTriplets = 
    length . 
    filter (
        \(KMeld m _) -> case m of
            Triplet (Tile Honor n) -> n `elem` [5,6,7]
            _ -> False)

-- shousangen _ _ = 0

-- 3 Han

-- half flush
honitsu :: HandContext -> AgariHand -> Int
honitsu ctx (Standard kmelds pair) =
    let allTiles =
          map (meldTile . baseMeld) kmelds
          ++ [pairTile pair]

        suits = nub [ s | Tile s _ <- allTiles, s /= Honor ]

    in fromEnum (length suits == 1) * (2 + fromEnum ( isMenzen ctx ))

honitsu _ _ = 0

-- twice pure double sequence
ryanpeikou ctx (Standard kmelds pair) 
    | not $ isMenzen ctx = 0
    | length ( filter (\m -> isSequenceMeld m && freq m == 2)  (nub kmelds)) == 2 = 3
    | otherwise = 0
        where freq y = length $ filter (== y) kmelds
ryanpeikou _ _ = 0

-- 6 han

-- full flush
chinitsu :: HandContext -> AgariHand -> Int
chinitsu ctx (Standard kmelds pair) =
    let allTiles =
          map (meldTile . baseMeld) kmelds
          ++ [pairTile pair]

        suits = nub [ s | Tile s _ <- allTiles, s /= Honor ]
        noHonor = all (\(Tile s _ ) -> s /= Honor) allTiles

    in fromEnum (length suits == 1) * (5 + fromEnum ( isMenzen ctx ))

chinitsu _ _ = 0


-- Yakuman

-- 13 + 1 terminal and honor
kokushi _ (Kokushi hand) = 13
kokushi _ _ = 0

-- 3 big dragon
daisangen _ (Standard kmelds pair) = 13 * fromEnum (countDragonTriplets kmelds == 3)
daisangen _ _ = 0

allYaku =
  [ menzenchin
  , riichi
  , pinfu
  , iipeikou
  , tanyao
  , yakuhai
  , chantaiyao
  , sanshoku
  , ittsu
  , toitoi
  , chiitoitsu
  , honroutou
  , shousangen
  , honitsu
  , ryanpeikou
  , chinitsu
  , kokushi
  , daisangen
  ]

calcHan :: HandContext -> AgariHand -> Int
calcHan ctx ag@(Chiitoi _) = 2
calcHan ctx ag@(Kokushi _) = 13      -- treat Kokushi as Yakuman (13 han)
calcHan ctx ag@(Standard _ _) =
    sum $ map (\f -> f ctx ag) allYaku

-----------------------------------------------------------------
-- Fu Calculator
-----------------------------------------------------------------

isTerminalOrHonor :: Tile -> Bool
isTerminalOrHonor (Tile Honor _) = True
isTerminalOrHonor (Tile _ 1)     = True
isTerminalOrHonor (Tile _ 9)     = True
isTerminalOrHonor _              = False

tilesOfMeld :: Meld -> [Tile]
tilesOfMeld (Triplet t)            = [t,t,t]
tilesOfMeld (Sequence (Tile s n))  = [Tile s n, Tile s (n+1), Tile s (n+2)]

tilesOfKMeld :: KMeld -> [Tile]
tilesOfKMeld (KMeld m _) = tilesOfMeld m

--------------------------------------------------------------------------------
-- Wait detection 
-- We detect: tanki (single-tile pair wait), kanchan (closed/middle wait),
-- penchan (edge wait: waiting for 3 to complete 1-2-3, or 7 to complete 7-8-9),
-- ryanmen (open-sided) is not fu (0), shanpon (double pair wait for a pair to become triplet)
-- Shanpon detection is nontrivial; we treat shanpon same as kanchan/penchan (i.e. +2)
--------------------------------------------------------------------------------

-- returns True if winTile is the pair tile (tanki)
isTankiWait :: Tile -> Pair -> Bool
isTankiWait wt (Pair p) = wt == p

-- find a Sequence Meld that contains the winTile, return (startN) if found
-- findSequenceContainingWinTile :: [KMeld] -> Tile -> Maybe Tile -- returns start tile of that sequence
-- findSequenceContainingWinTile kmelds wt =
--     case [ t | KMeld (Sequence t) _ <- kmelds, tileInSequence wt t ] of
--         (t:_) -> Just t
--         []    -> Nothing

-- tileInSequence :: Tile -> Tile -> Bool
-- tileInSequence (Tile s n) (Tile s2 n2) = s == s2 && n >= n2 && n <= n2 + 2

isKanchanOrPenchan wt kmelds =
    case findSequenceContainingWinTile kmelds wt of
      Nothing -> False
      Just (Sequence (Tile _ n)) ->
         let wn = numberTile wt in
         wn == n+1                     -- kanchan
         || (n == 1 && wn == 3)        -- penchan 1-2 waiting 3
         || (n == 7 && wn == 7)        -- penchan 8-9 waiting 7
      _ -> False


-- naive shanpon detection:
-- if winTile completes a triplet that exists as KMeld, then it's not a shanpon;
-- shanpon is when the wait was waiting on a pair that matched one of two different pairs
-- Implementing exact shanpon detection needs the pre-win partition; here we approximate:
isShanponWait :: Tile -> [KMeld] -> Bool
isShanponWait wt kmelds =
    -- simple heuristic: if winTile equals the meldTile of some Triplet in final hand,
    -- it's not a shanpon (it completed that triplet). For shanpon we would have a pair
    -- prior to winning; detection properly requires checking alternative partitions.
    False

-- Decide if wait gives +2 fu (tanki, kanchan, penchan, shanpon)
waitFu :: Tile -> [KMeld] -> Pair -> Int
waitFu wt kmelds pair
  | isTankiWait wt pair               = 2
  | isKanchanOrPenchan wt kmelds      = 2
  | isShanponWait wt kmelds           = 2
  | otherwise                         = 0

--------------------------------------------------------------------------------
-- Meld fu rules
-- Triplet (pon) fu:
--  closed simple triplet (no terminal/honor) = 4 fu
--  closed terminal/honor triplet = 8 fu
--  open simple triplet = 2 fu
--  open terminal/honor triplet = 4 fu
-- sequences give 0 fu
--------------------------------------------------------------------------------

meldFu :: KMeld -> Int
meldFu (KMeld (Sequence _) _) = 0
meldFu (KMeld (Triplet t) isOpen)
  | isOpen && isTerminalOrHonor t = 4
  | isOpen                        = 2
  | not isOpen && isTerminalOrHonor t = 8
  | otherwise                     = 4

pairFu :: Pair -> HandContext -> Int
pairFu (Pair t) ctx =
  case t of
    Tile Honor n | n >= 5 && n <= 7 -> 2                           -- dragons
    Tile Honor n                     -> if n == playerWind ctx || n == roundWind ctx then 2 else 0
    _                                 -> 0

--------------------------------------------------------------------------------
-- Main fu calculation
--------------------------------------------------------------------------------

-- round up fu according to riichi rules: except special 20-fu case
roundFu :: Int -> Int
roundFu fu
  | fu <= 20 = 20
  | otherwise = ((fu + 9) `div` 10) * 10

-- calcFu: compute fu for a given AgariHand and context
--
calcFu :: HandContext -> AgariHand -> Int
calcFu ctx@(HandContext { winTile, winMethod }) hand@(Chiitoi pairs)
  = 25   -- chiitoitsu fixed 25 fu

calcFu ctx@(HandContext { winTile, winMethod }) hand@(Kokushi _) =
  0      -- kokushi is yakuman; Fu usually not used. return 0 (handled elsewhere)

calcFu ctx@(HandContext { winTile, winMethod }) ag@(Standard kmelds pair) =
  let
    -- Special Pinfu/Tsumo rule:
    -- If pinfu and tsumo -> total fu is 20 (special-case)
    hasPinfu = pinfu ctx ag > 0
    isTsumoWin = case winMethod of { Tsumo -> True; _ -> False }
    isMenzenHand = isMenzen ctx

    -- if pinfu and tsumo -> special 20 fu
    specialPinfuTsumo = hasPinfu && isTsumoWin

    -- base 20 fu for normal hands (unless special pinfu tsumo handled above)
    base = 20

    -- sum of meld fu
    mf = sum (map meldFu kmelds)

    -- pair fu (yakuhai pair)
    pf = pairFu pair ctx

    -- wait fu (tanki/kanchan/penchan/shanpon)
    wf = waitFu winTile kmelds pair

    -- tsumo fu: +2 for tsumo except special pinfu tsumo
    tsumoBonus = if isTsumoWin && not specialPinfuTsumo then 2 else 0

    -- menzen ron bonus: +10 if ron and hand was menzen (closed)
    menzenRonBonus = case winMethod of
                       Ron -> if isMenzenHand then 10 else 0
                       _   -> 0

    -- total before rounding:
    totalRaw = if specialPinfuTsumo
                then 20   -- special-case pinfu tsumo: total fu = 20
                else base + mf + pf + wf + tsumoBonus + menzenRonBonus

    total = roundFu totalRaw
  in total

--------------------------------------------------------------------------------------------------
-- Score Calculation
--------------------------------------------------------------------------------------------------

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

ctxRon :: Tile -> HandContext
ctxRon wt = HandContext
  { winTile    = wt
  , winMethod  = Ron
  , isDealer   = False
  , playerWind = 1
  , roundWind  = 1
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
