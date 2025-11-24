-- {-# LANGUAGE NamedFieldPuns #-}

module YakuCalculator where

import Hand
import AgariCheck
import Data.Tuple.Extra
import ShantenCalculate
import qualified Data.Map.Strict as Map
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.MemoTrie 

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
       n == seatWind ctx
    || n == prevalentWind ctx
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
yakuhai ctx (Standard kmelds _) = length [ num | (KMeld (Triplet (Tile Honor num)) _) <- kmelds, honor <- [5,6,7,prevalentWind ctx, seatWind ctx], num == honor]
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


-- Mixed Triple Sequence
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
      [ (Manzu, n), (Pinzu, n), (Souzu, n)]

-- pure straight

ittsu :: HandContext -> AgariHand -> Int
ittsu ctx (Standard kmelds _) =
    let startsBySuit =
          [ (suit, n)
          | KMeld (Sequence (Tile suit n)) _ <- kmelds
          , suit /= Honor ]

        grouped =
          [ [ n | (s',n) <- startsBySuit, s' == s ]
          | s <- [Manzu, Pinzu, Souzu] ]

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
