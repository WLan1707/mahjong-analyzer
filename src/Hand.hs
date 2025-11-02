{-# LANGUAGE InstanceSigs #-}

module Hand where

import qualified Data.Map.Strict as Map
import qualified Data.Functor.Product as Map
import Data.Char ( digitToInt, isDigit )

-- Membuat ADT untuk jenis tile
data Suit = Manzu | Pinzu | Souzu | Honor
    deriving (Show, Eq, Ord, Enum, Bounded)

data Tile = Tile Suit Int
    deriving (Show, Eq)

instance Ord Tile where
    compare :: Tile -> Tile -> Ordering
    compare (Tile s1 n1) (Tile s2 n2)
        | s1 == s2   = compare n1 n2
        | otherwise  = compare s1 s2

data Meld 
    = Sequence Tile         -- Sequence M4 artinya M4, M5, M6
    | Triplet Tile          -- Triplet Z3 artinya Z3, Z3, Z3
    deriving (Show)

data PMeld 
    = MissingMiddle Tile    -- MissingMiddle P4 artinya ada P4 dan P6
    | MissingOut Tile       -- MissingOut S2 artinya ada S2 dan S3

newtype Pair = Pair Tile  -- Pair P3 artinya P3, P3

type Hand = [Tile]

-- Tangan kemenangan
data AgariHand 
    = Standard [Meld] Pair  -- 4 Meld + 1 Pair 
    | Chiitoi [Pair]        -- 7 Pair
    | Kokushi Hand          -- 13 + 1 Honor & Terminal

type HandCount = Map.Map Tile Int

handToCount :: Hand -> HandCount
handToCount hand = Map.fromListWith (+) [(tile, 1) | tile <- hand]

findPair :: HandCount -> [Pair]
findPair = map Pair . Map.keys . Map.filter (>= 2)

findTriplet :: HandCount -> [Meld]
findTriplet = map Triplet . Map.keys . Map.filter (>= 3)

-- Parsing dari String menjadi Hand

-- Fungsi pembantu untuk mengubah Char menjadi Suit
charToSuit :: Char -> Either String Suit
charToSuit 'm' = Right Manzu
charToSuit 'p' = Right Pinzu
charToSuit 's' = Right Souzu
charToSuit 'z' = Right Honor  -- 'z' untuk jīpai (honor)
charToSuit c   = Left ("Karakter suit tidak valid: " ++ [c])

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