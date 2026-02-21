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
    ) where

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
