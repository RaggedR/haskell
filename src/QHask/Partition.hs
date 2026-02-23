module QHask.Partition
    ( Partition(..)
    , emptyPartition
    , mkPartition
    , getPart
    , weight
    , partLen
    , conjugate
    , genPartitions
    , genCompatible
    , partitionsOf
    , containedIn
    , dominates
    , mergeParts
    , horizontalStrips
    ) where

import Data.List (sortBy)

-- | A partition: a weakly decreasing list of positive integers.
newtype Partition = Partition { getParts :: [Int] }
    deriving (Eq, Ord, Show)

-- | The empty partition.
emptyPartition :: Partition
emptyPartition = Partition []

-- | Smart constructor: validates weakly decreasing and all positive.
mkPartition :: [Int] -> Maybe Partition
mkPartition [] = Just emptyPartition
mkPartition ps
    | all (> 0) ps && isWeaklyDecreasing ps = Just (Partition ps)
    | otherwise = Nothing
  where
    isWeaklyDecreasing []       = True
    isWeaklyDecreasing [_]      = True
    isWeaklyDecreasing (x:y:xs) = x >= y && isWeaklyDecreasing (y:xs)

-- | 0-indexed part access. Returns 0 for out-of-bounds indices.
getPart :: Partition -> Int -> Int
getPart (Partition ps) j
    | j < 0 || j >= length ps = 0
    | otherwise                = ps !! j

-- | Sum of all parts.
weight :: Partition -> Int
weight (Partition ps) = sum ps

-- | Number of parts.
partLen :: Partition -> Int
partLen (Partition ps) = length ps

-- | Conjugate (transpose) of a partition.
-- conjugate [4,2,1] = [3,2,1,1]
conjugate :: Partition -> Partition
conjugate (Partition []) = emptyPartition
conjugate (Partition lam@(l0:_)) =
    Partition [length (filter (> i) lam) | i <- [0..l0-1]]

-- | Generate all partitions with largest part <= maxPart and total weight <= maxSize.
-- Includes the empty partition.
genPartitions :: Int -> Int -> [Partition]
genPartitions maxPart maxSize = map Partition (go maxPart maxSize)
  where
    go _ remaining | remaining < 0 = []
    go maxP remaining =
        [] : [ p : rest
             | p <- [min maxP remaining, min maxP remaining - 1 .. 1]
             , rest <- go p (remaining - p)
             ]

-- | Generate partitions compatible with a predecessor under shifted interlacing.
--
-- For shift s, the constraint is:
--   nu[k] <= nu_prev[k - s]  for k >= s
--   nu[k] <= maxPart          for k < s
--
-- This models the interlacing condition in cylindric partitions.
genCompatible :: Partition -> Int -> Int -> Int -> [Partition]
genCompatible nuPrev shift maxPart maxSize =
    map Partition (go 0 maxPart maxSize)
  where
    go _ _ remaining | remaining < 0 = []
    go pos prevVal remaining =
        let -- Upper bound from interlacing: nu[pos] <= nu_prev[pos - shift]
            interlaceBound = if pos >= shift
                             then getPart nuPrev (pos - shift)
                             else maxPart
            ub = min prevVal (min interlaceBound remaining)
        in if ub <= 0
           then [[]]  -- can only produce the empty tail
           else [] : [ p : rest
                     | p <- [ub, ub - 1 .. 1]
                     , rest <- go (pos + 1) p (remaining - p)
                     ]

-- | Does λ fit inside μ? i.e., λ_i ≤ μ_i for all i.
containedIn :: Partition -> Partition -> Bool
containedIn (Partition lam) (Partition mu) =
    length lam <= length mu && and (zipWith (<=) lam mu)

-- | Dominance order: λ dominates μ iff Σ_{i≤k} λ_i ≥ Σ_{i≤k} μ_i for all k.
dominates :: Partition -> Partition -> Bool
dominates lam mu =
    weight lam == weight mu &&
    all (>= 0) (zipWith (-) (partialSums lam) (partialSums mu))
  where
    n = max (partLen lam) (partLen mu)
    partialSums p = scanl1 (+) [getPart p i | i <- [0..n-1]]

-- | All partitions of exactly n.
partitionsOf :: Int -> [Partition]
partitionsOf 0 = [emptyPartition]
partitionsOf n
    | n < 0     = []
    | otherwise = map Partition (go n n)
  where
    go 0 _ = [[]]
    go _ maxP | maxP <= 0 = []
    go remaining maxP =
        [ p : rest
        | p <- [min remaining maxP, min remaining maxP - 1 .. 1]
        , rest <- go (remaining - p) p
        ]

-- | Merge two partitions by combining and re-sorting parts.
-- Used for multiplication in bases where product is indexing union (h, e, p).
mergeParts :: Partition -> Partition -> Partition
mergeParts (Partition ps) (Partition qs) =
    Partition $ sortBy (flip compare) (ps ++ qs)

-- | Enumerate all shapes ν such that ν/inner is a horizontal strip of size count,
-- with ν ⊆ outer.
--
-- Horizontal strip condition: inner_i ≤ ν_i for all i, and ν_i ≤ inner_{i-1}
-- for i ≥ 1 (at most one cell per column).
horizontalStrips :: Partition -> Partition -> Int -> [Partition]
horizontalStrips outer inner count
    | count < 0 = []
    | count == 0 = [inner]
    | otherwise = map (Partition . dropTrailingZeros) $ go 0 count
  where
    nRows = partLen outer

    go :: Int -> Int -> [[Int]]
    go row remaining
        | remaining < 0  = []
        | row >= nRows   = [[] | remaining == 0]
        | otherwise =
            [ newVal : rest
            | newVal <- [hi, hi - 1 .. lo]
            , rest <- go (row + 1) (remaining - (newVal - innerRow))
            ]
      where
        innerRow = getPart inner row
        lo = innerRow
        hi = minimum [ getPart outer row
                     , innerRow + remaining
                     , if row == 0 then getPart outer 0 else getPart inner (row - 1)
                     ]

    dropTrailingZeros = reverse . dropWhile (== 0) . reverse
