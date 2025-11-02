module AgariCheck where

import Hand
import qualified Data.Map.Strict as Map
import Data.List (foldl')

isSequence :: Tile -> HandCount -> Bool
isSequence (Tile suit num) handCount =
    suit /= Honor &&
    Map.findWithDefault 0 (Tile suit num) handCount > 0 &&
    Map.findWithDefault 0 (Tile suit (num + 1)) handCount > 0 &&
    Map.findWithDefault 0 (Tile suit (num + 2)) handCount > 0

removeTile :: HandCount -> Tile -> HandCount
removeTile handCount tile = Map.adjust (\c -> c - 1) tile handCount

removeOneTile :: Tile -> HandCount -> HandCount
removeOneTile =
    let updateFunc c = if c > 1 then Just (c - 1) else Nothing
        in Map.update updateFunc 

removePair :: Pair -> HandCount -> HandCount
removePair (Pair tile) hc =
    let tilesToRemove = replicate 2 tile
        in foldl' (flip removeOneTile) hc tilesToRemove

removeSequence :: Meld -> HandCount -> HandCount
removeSequence (Sequence (Tile suit num)) handCount = 
    let t1 = Tile suit num
        t2 = Tile suit (num + 1)
        t3 = Tile suit (num + 2)
        tilesToAdjust = [t1, t2, t3]

    in foldl' (flip removeOneTile) handCount tilesToAdjust

removeTriplet :: Meld -> HandCount -> HandCount
removeTriplet (Triplet tile) handCount = 
    let tilesToAdjust = replicate 3 tile

    in foldl' (flip removeOneTile) handCount tilesToAdjust 

checkMeld :: HandCount -> Int -> Bool
checkMeld hc meldToFind
    | meldToFind == 0 = Map.null hc
    | otherwise =
        case Map.minViewWithKey hc of
            Nothing -> False

            Just ((firstTile, count), _) ->
                
                let tryTriplet = 
                        (count >= 3) && 
                        let m = Triplet firstTile
                            hc' = removeTriplet m hc 
                        in checkMeld hc' (meldToFind - 1)

                    trySequence = 
                        isSequence firstTile hc &&
                        let m = Sequence firstTile
                            hc' = removeSequence m hc
                        in checkMeld hc' (meldToFind - 1)

                in tryTriplet || trySequence

isChiitoi :: HandCount -> Bool
isChiitoi hc = length (findPair hc) == 7

isKokushi :: HandCount -> Bool
isKokushi hc =
    let terminals = [ Tile s n | s <- [Manzu, Pinzu, Souzu], n <- [1,9] ]
        honors    = [ Tile Honor n | n <- [1..7] ]
        allOrphans = terminals ++ honors
        uniqueTiles = Map.keys hc
        hasAll = all (`elem` uniqueTiles) allOrphans
        hasPair = any (\t -> Map.findWithDefault 0 t hc >= 2) allOrphans
    in hasAll && hasPair

agariCheck :: HandCount -> Bool
agariCheck hc 
    | isChiitoi hc = True
    | isKokushi hc = True
    | otherwise =
        let possiblePair = findPair hc

            tryPair pair = 
                let handAfterPair = removePair pair hc
                in checkMeld handAfterPair 4

            in any tryPair possiblePair

-- Tes sederhana untuk agariCheck
hand1 = "55667789m56799s4m"
hand2 = "223344789m22456s"
hand3 = "4488m115599s1155z"
hand4 = "999m6677p3355678s"
hand5 = "456m456p45566778s"

agariTest = [agariCheckFromString hand | hand <- [hand1,hand2,hand3,hand4,hand5]] where
    agariCheckFromString :: String -> Either String Bool
    agariCheckFromString s = do
        hand <- parseHand s
        let result = agariCheck $ handToCount  hand
        return result
