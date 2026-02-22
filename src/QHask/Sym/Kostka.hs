module QHask.Sym.Kostka
    ( kostkaNumber
    , enumerateSSYT
    , inverseKostkaEntry
    ) where

import QHask.Partition

-- | Count semistandard Young tableaux of shape λ and content μ.
kostkaNumber :: Partition -> Partition -> Integer
kostkaNumber lam mu = fromIntegral $ length $ enumerateSSYT lam mu

-- | Enumerate all SSYT of shape λ and content μ.
-- Each SSYT is represented as a list of rows (each row is a list of Ints).
--
-- Algorithm: An SSYT is equivalent to a chain of partitions
-- ∅ = ν_0 ⊂ ν_1 ⊂ ... ⊂ ν_r = λ where each ν_k / ν_{k-1} is a horizontal
-- strip of size μ_k (and value k fills the cells of that strip).
enumerateSSYT :: Partition -> Partition -> [[[Int]]]
enumerateSSYT lam mu
    | weight lam /= weight mu = []
    | otherwise = map (chainToRows (partLen lam)) $ buildChain (getParts mu) 1 emptyPartition
  where
    buildChain :: [Int] -> Int -> Partition -> [[(Int, Partition, Partition)]]
    buildChain [] _ inner
        | inner == lam = [[]]
        | otherwise    = []
    buildChain (count:rest) val inner =
        [ (val, inner, nu) : chain
        | nu <- horizontalStrips lam inner count
        , chain <- buildChain rest (val + 1) nu
        ]

-- | Convert a chain of (value, oldShape, newShape) triples into a tableau.
chainToRows :: Int -> [(Int, Partition, Partition)] -> [[Int]]
chainToRows nRows chain =
    [ concatMap (\(val, old, new) ->
          replicate (getPart new i - getPart old i) val)
      chain
    | i <- [0 .. nRows - 1]
    ]

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

-- | Entry (λ,μ) of the inverse Kostka matrix K^{-1}.
-- K is upper-unitriangular in dominance order: K_{λ,μ} ≠ 0 only when λ dominates μ,
-- and K_{λ,λ} = 1. The inverse is computed by back-substitution:
-- (K^{-1})_{λ,μ} = -Σ_{ν: λ ▷ ν ≥ μ} K_{λ,ν} · (K^{-1})_{ν,μ}
inverseKostkaEntry :: Partition -> Partition -> Rational
inverseKostkaEntry lam mu
    | weight lam /= weight mu = 0
    | lam == mu               = 1
    | not (lam `dominates` mu) = 0
    | otherwise = negate $ sum
        [ fromIntegral (kostkaNumber lam rho) * inverseKostkaEntry rho mu
        | rho <- partitionsOf (weight lam)
        , rho /= lam
        , lam `dominates` rho
        , rho `dominates` mu
        ]
