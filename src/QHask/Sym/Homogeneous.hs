{-# OPTIONS_GHC -Wno-orphans #-}
module QHask.Sym.Homogeneous
    ( h
    , mulH
    ) where

import qualified Data.Map.Strict as Map

import QHask.Partition (Partition(..), mergeParts, partitionsOf, weight)
import QHask.Sym.Types
import QHask.Sym.Kostka (kostkaNumber, inverseKostkaEntry)

-- | Complete homogeneous basis element h_μ.
h :: Partition -> SymFun Homo
h mu = SymFun (Map.singleton mu 1)

-- | Multiply homogeneous symmetric functions: h_μ · h_ν = h_{μ∪ν}
-- where μ∪ν is the partition obtained by merging and sorting parts.
mulH :: SymFun Homo -> SymFun Homo -> SymFun Homo
mulH (SymFun a) (SymFun b) = SymFun $ Map.filter (/= 0) $
    Map.fromListWith (+)
        [ (mergeParts mu nu, ca * cb)
        | (mu, ca) <- Map.toList a
        , (nu, cb) <- Map.toList b
        ]

instance Basis Homo where
    basisName _ = "h"
    -- h_μ = Σ_λ K_{λ,μ} s_λ
    toSchur (SymFun coeffs) = SymFun $ Map.filter (/= 0) $
        Map.fromListWith (+)
            [ (lam, c * fromIntegral (kostkaNumber lam mu))
            | (mu, c) <- Map.toList coeffs
            , lam <- partitionsOf (weight mu)
            ]
    -- s → h: b_μ = Σ_λ (K^{-1})_{μ,λ} a_λ
    fromSchur (SymFun coeffs) = SymFun $ Map.filter (/= 0) $
        Map.fromListWith (+)
            [ (mu, c * inverseKostkaEntry mu lam)
            | (lam, c) <- Map.toList coeffs
            , mu <- partitionsOf (weight lam)
            ]
