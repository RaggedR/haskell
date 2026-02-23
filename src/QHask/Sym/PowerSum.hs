{-# OPTIONS_GHC -Wno-orphans #-}
module QHask.Sym.PowerSum
    ( p
    , mulP
    , zCoeff
    , character
    ) where

import Data.List (group, sort)
import qualified Data.Map.Strict as Map

import QHask.Partition (Partition(..), mergeParts, partitionsOf, weight)
import QHask.Sym.Types

-- | Power sum basis element p_μ.
p :: Partition -> SymFun Pow
p mu = SymFun (Map.singleton mu 1)

-- | Multiply power sum symmetric functions: p_μ · p_ν = p_{μ∪ν}.
mulP :: SymFun Pow -> SymFun Pow -> SymFun Pow
mulP (SymFun a) (SymFun b) = SymFun $ Map.filter (/= 0) $
    Map.fromListWith (+)
        [ (mergeParts mu nu, ca * cb)
        | (mu, ca) <- Map.toList a
        , (nu, cb) <- Map.toList b
        ]

-- | The z_μ coefficient: z_μ = Π_i i^{m_i} · m_i!
-- where m_i is the multiplicity of i in μ.
zCoeff :: Partition -> Rational
zCoeff (Partition []) = 1
zCoeff (Partition ps) = fromIntegral $ product
    [ i ^ mi * factorial mi
    | grp <- group (sort ps)
    , let i  = head grp
    , let mi = length grp
    ]
  where
    factorial n = product [1..n]

-- | Character χ^λ(μ) of the symmetric group, computed via the
-- Murnaghan-Nakayama rule.
--
-- χ^λ(μ) = Σ over border strips B of size μ_1 in λ, (-1)^{height(B)} · χ^{λ\B}(μ')
-- where μ' = (μ_2, μ_3, ...).
character :: Partition -> Partition -> Rational
character lam mu
    | weight lam /= weight mu = 0
    | null (getParts mu)      = if null (getParts lam) then 1 else 0
    | otherwise = sum
        [ (-1) ^ ht * character lam' mu'
        | (lam', ht) <- borderStrips lam r
        ]
  where
    r  = head (getParts mu)
    mu' = Partition (tail (getParts mu))

-- | Find all border strips (rim hooks) of size r in partition λ.
-- Returns (λ\B, height(B)) for each border strip B.
--
-- A border strip spanning rows s to e has:
--   Single-row (s=e): μ_s = λ_s - r, height = 0
--   Multi-row (s<e):  μ_i = λ_{i+1} - 1 for s ≤ i < e
--                     μ_e = λ_s + (e-s) - r
--                     height = e - s
borderStrips :: Partition -> Int -> [(Partition, Int)]
borderStrips (Partition []) _ = []
borderStrips lam r = singleRowStrips ++ multiRowStrips
  where
    parts = getParts lam
    n = length parts
    partAt i = if i < n then parts !! i else 0

    singleRowStrips =
        [ (mkPartition muParts, 0)
        | s <- [0 .. n - 1]
        , let muS = parts !! s - r
        , muS >= 0
        , muS >= partAt (s + 1)     -- partition condition: μ_s ≥ λ_{s+1}
        , let muParts = [if i == s then muS else parts !! i | i <- [0..n-1]]
        ]

    multiRowStrips =
        [ (mkPartition muParts, e - s)
        | s <- [0 .. n - 1]
        , e <- [s + 1 .. n - 1]
        , partAt (s + 1) >= 1       -- need μ_s = λ_{s+1}-1 ≥ 0
        , let muE = parts !! s + (e - s) - r
        , muE >= 0
        , muE >= partAt (e + 1)     -- partition condition: μ_e ≥ λ_{e+1}
        , muE <= parts !! e - 1     -- partition condition: μ_{e-1} ≥ μ_e
        , let muParts = [ if i < s then parts !! i
                          else if i < e then partAt (i + 1) - 1
                          else if i == e then muE
                          else parts !! i
                        | i <- [0..n-1]
                        ]
        ]

    mkPartition ps = Partition $ reverse $ dropWhile (<= 0) $ reverse ps

instance Basis Pow where
    basisName _ = "p"
    -- p_μ = Σ_λ χ^λ(μ) s_λ
    toSchur (SymFun coeffs) = SymFun $ Map.filter (/= 0) $
        Map.fromListWith (+)
            [ (lam, c * character lam mu)
            | (mu, c) <- Map.toList coeffs
            , lam <- partitionsOf (weight mu)
            ]
    -- s_λ = Σ_μ (1/z_μ) χ^λ(μ) p_μ
    fromSchur (SymFun coeffs) = SymFun $ Map.filter (/= 0) $
        Map.fromListWith (+)
            [ (mu, c * character lam mu / zCoeff mu)
            | (lam, c) <- Map.toList coeffs
            , mu <- partitionsOf (weight lam)
            ]
