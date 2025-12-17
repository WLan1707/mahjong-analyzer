# Lesson Learn

## 1. Pemisahan ADT: Pair, Meld, PartialMeld

Pemrosesan data mahjong menjadi stabil ketika setiap konstruksi domain dipisahkan ke dalam tipe data mandiri karena masing-masing memiliki aturan dan konteks berbeda. `Pair` hanya muncul satu kali sebagai kepala, `Meld` digunakan untuk yaku/fu/scoring, dan `PartialMeld` hanya relevan selama perhitungan shanten. Pemisahan ini membuat fungsi analisis tidak dapat menerima bentuk data yang salah karena dicegah oleh kompiler.

```haskell
data Suit = Manzu | Pinzu | Souzu | Honor

data Tile = Tile {
    suitTile :: Suit
    , numberTile :: Int}

data Meld 
    = Sequence {meldTile :: Tile}         -- Sequence M4 artinya M4, M5, M6
    | Triplet {meldTile :: Tile}          -- Triplet Z3 artinya Z3, Z3, Z3

data PMeld 
    = MissMid Tile    -- MissingMiddle P4 artinya ada P4 dan P6
    | MissOut Tile       -- MissingOut S2 artinya ada S2 dan S3

newtype Pair = Pair {pairTile :: Tile}  -- Pair P3 artinya P3, P3

```

<!-- ### Contoh fungsi

```haskell
fuForMeld :: Meld -> Int
fuForMeld (Pon _ _ _) = 2
fuForMeld (Chi _ _ _) = 0
fuForMeld (Kan _ _ _ _) = 8
``` -->

---

## 2. MemoTree sebagai Cache Rekursif Terstruktur

Ruang pencarian dalam perhitungan shanten bersifat eksponensial dan banyak subkonfigurasi `HandCount` muncul ulang. 
Alih-alih menyimpan hasil perhitungan dalam tabel, Haskell membangun sebuah pohon tak hingga yang setiap nodenya mewakili hasil dari suatu fungsi pada indeks tertentu.
1. Buat pohon biner tak hingga yang berisi bilangan bulat tak negatif $0,1,2,3,\ldots$ di setiap node.
2. Terapkan *map* fungsi `f` ke setiap node secara langsung sehingga sekarang setiap node berisi nilai fungsi `f` pada indeksnya. Memo terbentuk.
3. Karena lazy evaluation, hal ini dapat terjadi dengan tanpa ada perhitungan sama sekali.
4. Untuk mendapatkan nilai `f n`, cukup periksa node ke `n` dari tree, yang dapat dilakukan dalam $O(\log n)$. Pada saat ini fungsi `f n` baru dihitung dan akan langsung melihat nilai `f m` yang dibutuhkan.

### Struktur dasar

```haskell
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
```

dari fungsi shanten tanpa `memoTree`

```Haskell
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
```

Karena `memoTree` yang diinginkan harus bertipe
```Haskell
memoTree :: ((Int -> a) -> Int -> a) -> Int -> a
```
lakukan decoding dan encoding dari `HandCount` menjadi bilangan bulat `Int`.
Satu suit memiliki 9 nomor yang masing-masing tidak lebih dari 4 tile, sehingga list banyak tile dapat dianggap sebagai bilangan basis 5.

```Haskell
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
```
### Integrasi dengan perhitungan Shanten

```haskell
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
```

---

## 3. QuickCheck Monad Generator untuk Data Mahjong Terstruktur

QuickCheck adalah library Haskell untuk *property-based testing*. Alih-alih menulis test case satu per satu, developer mendefinisikan *property* (kondisi yang harus selalu benar), lalu QuickCheck menghasilkan banyak data acak untuk menguji property tersebut. Jika ada satu contoh yang membuat property gagal, QuickCheck akan menampilkan *counterexample*.

Generator QuickCheck dibangun menggunakan monad `Gen`, yaitu representasi fungsi yang menerima sumber angka acak serta parameter ukuran, lalu menghasilkan nilai secara deterministik. Dengan sifat monadik, generator dapat disusun secara bertahap: setiap langkah pembangkitan data dapat bergantung pada hasil langkah sebelumnya tanpa kehilangan purity.

Secara konseptual, instance monad untuk `Gen` dapat digambarkan sebagai berikut:

```haskell
instance Monad Gen where
  return x = Gen (\_ _ -> x)

  (Gen ga) >>= f =
    Gen $ \r n ->
      let (r1, r2) = split r
          a        = ga r1 n
          Gen gb   = f a
      in gb r2 n
```

Struktur ini menjelaskan bahwa komposisi generator dilakukan dengan membagi seed pseudo-random menjadi dua bagian untuk menjaga determinisme, menjalankan generator pertama, dan meneruskan hasilnya untuk membentuk generator berikutnya. Dengan cara ini, pembentukan data kompleks tetap dapat dijelaskan secara deklaratif dan bebas efek samping.

Generator data acak untuk mahjong harus mematuhi batasan domain (maksimal empat salinan per tile, total tile 13/14, validitas bentuk). Dengan monad `Gen`, data dibangun bertahap sambil mempertahankan keterbatasan tersebut, sehingga cocok diterapkan untuk pengujian property-based.

Dalam konteks mahjong:

- QuickCheck dapat menghasilkan berbagai bentuk tangan.
- Properti seperti “shanten tidak boleh negatif” dapat diuji pada ratusan hingga ribuan data acak.
- Ini memungkinkan deteksi bug yang tidak terlihat dalam unit test biasa.

### Mengapa memakai Monad Gen?

Generator QuickCheck ditulis dalam monad `Gen`. Alasannya:

1. **Komposisi bertahap**  
   Data dapat dibentuk sedikit demi sedikit sambil menjaga constraint:  
   contoh: memastikan tile tidak lebih dari 4 salinan.

2. **Efek pseudo-random yang terkontrol**  
   Monad memungkinkan generator membawa state internal produksi angka acak secara aman dan terstruktur.

3. **Struktur sequencing yang eksplisit**  
   Dapat ditulis:

```haskell
instance Arbitrary Suit where
  arbitrary = elements [Manzu, Pinzu, Souzu, Honor]

genTile :: Gen Tile
genTile = do
  suit <- arbitrary
  n <- case suit of
    Honor -> choose (1,7)
    _ -> choose (1,9)
  return (Tile suit n)

instance Arbitrary Tile where
  arbitrary = genTile
```

`do`-notation di atas hanyalah sintaks monad untuk menggabungkan langkah-langkah pengacakan sekaligus menjaga validitas domain.

### Contoh generator

```haskell
genSequence :: Gen Meld
genSequence = do
  suit <- elements [Manzu, Pinzu, Souzu]
  n    <- choose (1,7)
  return (Sequence (Tile suit n))

genTriplet :: Gen Meld
genTriplet = Triplet <$> genTile

instance Arbitrary Meld where
  arbitrary = oneof [genSequence, genTriplet]

instance Arbitrary KMeld where
  arbitrary = KMeld <$> arbitrary <*> arbitrary
```

### Properti contoh

```haskell
prop_scorePermutationInvariant :: HandContext -> AgariHand -> Property
prop_scorePermutationInvariant ctx ag =
  let h  = agariToHand ag
      sc = calcScore ctx ag
  in forAll (shuffle h) $ \h2 ->
       let hc2 = handToCount h2
           ag2s = findPartition hc2 []
       in null ag2s || maximum (map (calcScore ctx) ag2s) == sc
```

QuickCheck akan mencoba ratusan tangan dan permutasinya; jika ada kasus yang membuat nilai skor yang didapat dari mempermutasikan tangan berbeda dengan nilai awal, QuickCheck akan melaporkan tangan tersebut, dan developer dapat melakukan debugging.

---
