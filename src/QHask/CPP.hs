module QHask.CPP
    ( CPP(..)
    , GKDict
    , computeGK
    , enumerateCPPs
    , extractZCoeff
    , gkAtZ1
    ) where

import qualified Data.Map.Strict as Map

import QHask.Partition
import QHask.Profile

-- | A rank-3 cylindric plane partition.
data CPP = CPP
    { cppNu1     :: !Partition
    , cppNu2     :: !Partition
    , cppNu3     :: !Partition
    , cppProfile :: !Profile
    } deriving (Eq, Show)

-- | GK generating function: Map (maxEntry, weight) count.
type GKDict = Map.Map (Int, Int) Integer

-- | Check cyclic closure: nu3[j] >= nu1[j + c0] for all j,
-- and len(nu1) <= len(nu3) + c0.
cyclicClosure :: Int -> Partition -> Partition -> Bool
cyclicClosure c0 nu3 nu1 =
    partLen nu1 <= partLen nu3 + c0
    && all (\j -> getPart nu3 j >= getPart nu1 (j + c0)) [0 .. maxJ]
  where
    maxJ = max (partLen nu3 - 1) (partLen nu1 - c0 - 1)

-- | Compute GK_c(z,q) = sum_{CPP} z^{max entry} * q^{weight}.
-- Returns a map from (maxEntry, weight) to count.
computeGK :: Profile -> Int -> Int -> GKDict
computeGK prof maxN maxW =
    Map.fromListWith (+)
        [ ((mx, total), 1)
        | nu1 <- genPartitions maxN maxW
        , let s1 = weight nu1
        , nu2 <- genCompatible nu1 (profC1 prof) maxN (maxW - s1)
        , let s2 = weight nu2
        , nu3 <- genCompatible nu2 (profC2 prof) maxN (maxW - s1 - s2)
        , let s3 = weight nu3
        , cyclicClosure (profC0 prof) nu3 nu1
        , let total = s1 + s2 + s3
        , let mx = maximum0 [getPart nu1 0, getPart nu2 0, getPart nu3 0]
        ]
  where
    maximum0 [] = 0
    maximum0 xs = maximum xs

-- | Enumerate all CPPs with given max entry and max weight.
enumerateCPPs :: Profile -> Int -> Int -> [CPP]
enumerateCPPs prof maxN maxW =
    [ CPP nu1 nu2 nu3 prof
    | nu1 <- genPartitions maxN maxW
    , let s1 = weight nu1
    , nu2 <- genCompatible nu1 (profC1 prof) maxN (maxW - s1)
    , let s2 = weight nu2
    , nu3 <- genCompatible nu2 (profC2 prof) maxN (maxW - s1 - s2)
    , cyclicClosure (profC0 prof) nu3 nu1
    ]

-- | Extract coefficients of z^m: returns Map weight count.
extractZCoeff :: GKDict -> Int -> Map.Map Int Integer
extractZCoeff gk m =
    Map.fromListWith (+)
        [ (w, c)
        | ((n, w), c) <- Map.toList gk
        , n == m
        ]

-- | Evaluate GK at z=1: sum over all max entries.
-- Returns coefficient list indexed by weight.
gkAtZ1 :: GKDict -> Int -> [Integer]
gkAtZ1 gk maxW =
    [ Map.findWithDefault 0 w totals
    | w <- [0..maxW]
    ]
  where
    totals = Map.fromListWith (+)
        [ (w, c)
        | ((_, w), c) <- Map.toList gk
        ]
