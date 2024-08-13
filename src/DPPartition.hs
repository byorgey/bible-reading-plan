{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DPPartition where

import Control.Arrow ((&&&))
import Data.Array
import Data.List (minimumBy)
import Data.Ord (comparing)

data Cost a = Cost
  { lengthPenalty :: Int -> Int
  -- ^ Compute a penalty, given an amount over (+) or under (-) target length for each part
  , breakPenalty :: a -> a -> Int
  -- ^ Compute a penalty for breaking in between two given elements
  }

{-

P(i,k) = (p, c)
  where
    p = best way to partition items a[i..] into exactly k parts
    c = cost

P(n,0) = ([], 0)
P(n,_) = ([], INF)
P(i,0) = ([], INF)
P(i,k) = try all initial segments up to leaving exactly k-1 elements
  compute length + break penalties for each, add to P(i+j, k-1)

-}

inf :: Int
inf = 1000000000

bestPartition :: forall a. Cost a -> Int -> [a] -> [[a]]
bestPartition Cost {..} numParts as
  | n < numParts = map (: []) as
  | otherwise = fst (p (0, numParts))
 where
  n = length as
  targetLen = n `div` numParts
  a :: Array Int a
  a = listArray (0, n - 1) as

  p :: (Int, Int) -> ([[a]], Int)
  p = memo ((0, 0), (n, numParts)) $ \case
    (i, 0) | i == n -> ([], 0)
    (i, _) | i == n -> ([], inf)
    (_, 0) -> ([], inf)
    (i, k) ->
      minimumBy
        (comparing snd)
        [ (seg : rest, restCost + lengthPenalty (j - targetLen) + if i + j == n then 0 else breakPenalty (a ! (i + j - 1)) (a ! (i + j)))
        | j <- [1 .. n - i - k + 1]
        , let (rest, restCost) = p (i + j, k - 1)
        , let seg = map (a !) [i .. i + j - 1]
        ]

-- i - j >= k - 1

toTable :: Ix i => (i, i) -> (i -> a) -> Array i a
toTable rng f = array rng (map (id &&& f) (range rng))

fromTable :: Ix i => Array i a -> (i -> a)
fromTable = (!)

memo :: Ix i => (i, i) -> (i -> a) -> (i -> a)
memo rng = fromTable . toTable rng
