{-# LANGUAGE InstanceSigs #-}

module Hand where

import qualified Data.Map.Strict as Map
import qualified Data.Functor.Product as Map
import Data.Char ( digitToInt, isDigit )
import Text.ParserCombinators.ReadP (char)

-- Membuat ADT untuk jenis tile
data Suit = Manzu | Pinzu | Souzu | Honor
    deriving (Show, Eq, Ord, Enum, Bounded)

data Tile = Tile {
    suitTile :: Suit
    , numberTile :: Int
}
    deriving (Show, Eq)

instance Ord Tile where
    compare :: Tile -> Tile -> Ordering
    compare (Tile s1 n1) (Tile s2 n2)
        | s1 == s2   = compare n1 n2
        | otherwise  = compare s1 s2

data Meld 
    = Sequence {meldTile :: Tile}         -- Sequence M4 artinya M4, M5, M6
    | Triplet {meldTile :: Tile}          -- Triplet Z3 artinya Z3, Z3, Z3
    deriving (Show, Eq)

data PMeld 
    = MissMid Tile    -- MissingMiddle P4 artinya ada P4 dan P6
    | MissOut Tile       -- MissingOut S2 artinya ada S2 dan S3
    deriving (Show, Eq)

newtype Pair = Pair {pairTile :: Tile}  -- Pair P3 artinya P3, P3

data KMeld = KMeld {
    baseMeld :: Meld
    , isOpen :: Bool    -- apakah hasil call
} deriving (Show, Eq)

type Hand = [Tile]

-- Tangan kemenangan
data AgariHand 
    = Standard [KMeld] Pair  -- 4 Meld + 1 Pair 
    | Chiitoi [Pair]        -- 7 Pair
    | Kokushi Hand          -- 13 + 1 Honor & Terminal

type HandCount = Map.Map Tile Int

handToCount :: Hand -> HandCount
handToCount hand = Map.fromListWith (+) [ (tile, 1) | tile <- hand ]

countToHand :: HandCount -> Hand
countToHand hc = concat [ replicate n tile | (tile, n) <- Map.toList hc ]

findPair :: HandCount -> [Pair]
findPair = map Pair . Map.keys . Map.filter (>= 2)

findTriplet :: HandCount -> [Meld]
findTriplet = map Triplet . Map.keys . Map.filter (>= 3)

isPair :: Tile -> HandCount -> Bool
isPair tile handCount = Map.findWithDefault 0 tile handCount > 1

isTriplet :: Tile -> HandCount -> Bool
isTriplet tile handCount = Map.findWithDefault 0 tile handCount > 2

isSequence :: Tile -> HandCount -> Bool
isSequence (Tile suit num) handCount =
    suit /= Honor &&
    Map.findWithDefault 0 (Tile suit num) handCount > 0 &&
    Map.findWithDefault 0 (Tile suit (num + 1)) handCount > 0 &&
    Map.findWithDefault 0 (Tile suit (num + 2)) handCount > 0

isMissOut :: Tile -> HandCount -> Bool
isMissOut (Tile suit num) hand =
    suit /= Honor &&
    Map.findWithDefault 0 (Tile suit num) hand > 0 &&
    Map.findWithDefault 0 (Tile suit (num + 1)) hand > 0

isMissMid :: Tile -> HandCount -> Bool
isMissMid (Tile suit num) handCount =
    suit /= Honor &&
    Map.findWithDefault 0 (Tile suit num) handCount > 0 &&
    Map.findWithDefault 0 (Tile suit (num + 2)) handCount > 0

findSequence :: HandCount -> [Meld]
findSequence handCount = [Sequence tile | tile <- Map.keys handCount, isSequence tile handCount]

findMissOut :: HandCount -> [PMeld]
findMissOut hand = [MissOut tile | tile <- Map.keys hand, isMissOut tile hand]

findMissMid :: HandCount -> [PMeld]
findMissMid hand = [MissMid tile | tile <- Map.keys hand, isMissMid tile hand]

-- Parsing dari String menjadi Hand

-- Fungsi pembantu untuk mengubah Char menjadi Suit
charToSuit :: Char -> Either String Suit
-- charToSuit 'm' = Right Manzu
-- charToSuit 'p' = Right Pinzu
-- charToSuit 's' = Right Souzu
-- charToSuit 'z' = Right Honor  -- 'z' untuk jīpai (honor)
-- charToSuit c   = Left ("Karakter suit tidak valid: " ++ [c])

charToSuit char
    | char == 'm' = Right Manzu
    | char == 'p' = Right Pinzu
    | char == 's' = Right Souzu
    | char == 'z' = Right Honor
    | otherwise = Left ("Karakter suit tidak valid: " ++ [char])

-- Fungsi pembantu untuk mengubah String angka menjadi [Tile]
-- "123" -> Manzu -> [Tile Manzu 1, Tile Manzu 2, Tile Manzu 3]
stringToTiles :: String -> Suit -> [Tile]
stringToTiles nums suit =
    map (Tile suit . digitToInt) nums

-- Fungsi Parsing Utama
parseHand :: String -> Either String Hand
parseHand "" = Right []  -- Base case: string kosong adalah tangan kosong
parseHand str =
    -- Pisahkan grup angka di depan
    -- span isDigit "123m456p" -> ("123", "m456p")
    let (nums, rest) = span isDigit str
    in
        if null nums then
            Left "Input tidak valid: Diharapkan ada angka."
        else if null rest then
            Left "Input tidak valid: Diharapkan ada suit (m,p,s,z) setelah angka."
        else
            let suitChar = head rest
                remainingString = tail rest
            in
                case charToSuit suitChar of
                    Left err   -> Left err
                    Right suit ->
                        -- Buat tiles untuk bagian ini
                        let currentTiles = stringToTiles nums suit
                        -- Panggil rekursi untuk sisa string
                        in (currentTiles ++) <$> parseHand remainingString