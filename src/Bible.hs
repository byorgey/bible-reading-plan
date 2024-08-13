{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Bible where

import Control.Monad.State
import DPPartition
import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Extra (enumerate)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

data Book
  = Genesis
  | Exodus
  | Leviticus
  | Numbers
  | Deuteronomy
  | Joshua
  | Judges
  | Ruth
  | Samuel1
  | Samuel2
  | Kings1
  | Kings2
  | Ezra
  | Nehemiah
  | Esther
  | Job
  | Psalm
  | Proverbs
  | Ecclesiastes
  | SongOfSongs
  | Chronicles1
  | Chronicles2
  | Isaiah
  | Jeremiah
  | Lamentations
  | Ezekiel
  | Daniel
  | Hosea
  | Joel
  | Amos
  | Obadiah
  | Jonah
  | Micah
  | Nahum
  | Habakkuk
  | Zephaniah
  | Haggai
  | Zechariah
  | Malachi
  | Matthew
  | Mark
  | Luke
  | Acts
  | John
  | Romans
  | Corinthians1
  | Corinthians2
  | Galatians
  | Ephesians
  | Philippians
  | Colossians
  | Thessalonians1
  | Thessalonians2
  | Timothy1
  | Timothy2
  | Titus
  | Philemon
  | Hebrews
  | James
  | Peter1
  | Peter2
  | John1
  | John2
  | John3
  | Jude
  | Revelation
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

data Group
  = Pentateuch
  | PreExHist
  | PreExPro
  | Writings
  | PostExPro
  | NTHist
  | Epistles
  | Psalms
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

booksOf :: Map Group (Set Book)
booksOf =
  M.fromList
    [ Pentateuch
        ==> [ Genesis
            , Exodus
            , Leviticus
            , Numbers
            , Deuteronomy
            ]
    , PreExHist
        ==> [ Joshua
            , Judges
            , Ruth
            , Samuel1
            , Samuel2
            , Kings1
            , Kings2
            ]
    , Writings
        ==> [ Ezra
            , Nehemiah
            , Esther
            , Job
            , Proverbs
            , Ecclesiastes
            , SongOfSongs
            , Chronicles1
            , Chronicles2
            ]
    , PreExPro
        ==> [ Isaiah
            , Jeremiah
            , Hosea
            , Joel
            , Amos
            , Obadiah
            , Jonah
            , Micah
            , Nahum
            , Habakkuk
            , Zephaniah
            ]
    , PostExPro
        ==> [ Lamentations
            , Ezekiel
            , Daniel
            , Haggai
            , Zechariah
            , Malachi
            , Revelation
            ]
    , NTHist
        ==> [ Matthew
            , Mark
            , Luke
            , Acts
            , John
            ]
    , Epistles
        ==> [ Romans
            , Corinthians1
            , Corinthians2
            , Galatians
            , Ephesians
            , Philippians
            , Colossians
            , Thessalonians1
            , Thessalonians2
            , Timothy1
            , Timothy2
            , Titus
            , Philemon
            , Hebrews
            , James
            , Peter1
            , Peter2
            , John1
            , John2
            , John3
            , Jude
            ]
    , Psalms ==> [Psalm]
    ]
 where
  g ==> bs = (g, S.fromList bs)

groupOf :: Map Book Group
groupOf = M.fromList . concatMap (\(g, bs) -> map (,g) (S.toList bs)) . M.assocs $ booksOf

------------------------------------------------------------
-- Ensure we have properly partitioned the books into groups

prop_groups_partition :: Bool
prop_groups_partition = prop_groups_disjoint && prop_groups_union

prop_groups_union :: Bool
prop_groups_union = S.unions (map (booksOf !) (enumerate @Group)) == S.fromList (enumerate @Book)

prop_groups_disjoint :: Bool
prop_groups_disjoint = all disjoint [(g1, g2) | g1 <- enumerate @Group, g2 <- enumerate @Group, g1 /= g2]
 where
  disjoint (a, b) = (booksOf ! a) `S.intersection` (booksOf ! b) == S.empty

------------------------------------------------------------

numChapters :: Book -> Int
numChapters = \case
  Genesis -> 50
  Exodus -> 40
  Leviticus -> 27
  Numbers -> 36
  Deuteronomy -> 34
  Joshua -> 24
  Judges -> 21
  Ruth -> 4
  Samuel1 -> 31
  Samuel2 -> 24
  Kings1 -> 22
  Kings2 -> 25
  Ezra -> 10
  Nehemiah -> 13
  Esther -> 10
  Job -> 42
  Proverbs -> 31
  Ecclesiastes -> 12
  SongOfSongs -> 8
  Chronicles1 -> 29
  Chronicles2 -> 36
  Isaiah -> 66
  Jeremiah -> 52
  Hosea -> 14
  Joel -> 3
  Amos -> 9
  Obadiah -> 1
  Jonah -> 4
  Micah -> 7
  Nahum -> 3
  Habakkuk -> 3
  Zephaniah -> 3
  Lamentations -> 5
  Ezekiel -> 48
  Daniel -> 12
  Haggai -> 2
  Zechariah -> 14
  Malachi -> 4
  Revelation -> 22
  Matthew -> 28
  Mark -> 16
  Luke -> 24
  Acts -> 28
  John -> 21
  Romans -> 16
  Corinthians1 -> 16
  Corinthians2 -> 13
  Galatians -> 6
  Ephesians -> 6
  Philippians -> 4
  Colossians -> 4
  Thessalonians1 -> 5
  Thessalonians2 -> 3
  Timothy1 -> 6
  Timothy2 -> 4
  Titus -> 3
  Philemon -> 1
  Hebrews -> 13
  James -> 5
  Peter1 -> 5
  Peter2 -> 3
  John1 -> 5
  John2 -> 1
  John3 -> 1
  Jude -> 1
  Psalm -> 150

prop_num_chapters :: Bool
prop_num_chapters = sum (map numChapters (enumerate @Book)) == 1189

groupNumChapters :: Group -> Int
groupNumChapters = sum . map numChapters . S.toList . (booksOf !)

type Chapter = (Book, Int)

chapters :: Book -> [Chapter]
chapters b = map (b,) [1 .. numChapters b]

groupChapters :: Group -> [Chapter]
groupChapters = concatMap chapters . S.toList . (booksOf !)

------------------------------------------------------------
-- Construct reading plan

lenCost :: Int -> Int
lenCost = (^ 2) . abs

breakCost :: Chapter -> Chapter -> Int
breakCost (a, _) (b, _) = if a /= b then 0 else 10

partitionGroup :: Group -> [[Chapter]]
partitionGroup = bestPartition (Cost lenCost breakCost) 24 . groupChapters

readingPlan :: [[Chapter]]
readingPlan = flip evalState (chapters Psalm) . mapM interleaveS . transpose $ map partitionGroup [Pentateuch .. Epistles]
 where
  interleave [] xs = xs
  interleave (p : ps) (x : xs) = [p] : x : interleave ps xs

next :: State [a] (Maybe a)
next = do
  as <- get
  case as of
    a : t -> do
      put t
      pure (Just a)
    _ -> pure Nothing

interleaveS :: [[a]] -> State [a] [a]
interleaveS [] = pure []
interleaveS (g : gs) = (++) . maybe g (: g) <$> next <*> interleaveS gs

------------------------------------------------------------
-- Format reading plan for Roam

roamReadingPlan :: String
roamReadingPlan = unlines . concat $ zipWith formatCycle [1 ..] readingPlan

formatCycle :: Int -> [Chapter] -> [String]
formatCycle n cs =
  ("- Cycle " ++ show n) : map (("  - " ++) . formatChapter) cs

formatChapter :: Chapter -> String
formatChapter (bk, c) = "[[" ++ formatBook bk ++ " " ++ show c ++ "]]"

formatBook :: Book -> String
formatBook = \case
  SongOfSongs -> "Song of Songs"
  bk
    | isDigit (last (show bk)) -> last (show bk) : " " ++ init (show bk)
    | otherwise -> show bk
