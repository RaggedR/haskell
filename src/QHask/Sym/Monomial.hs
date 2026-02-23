{-# OPTIONS_GHC -Wno-orphans #-}
module QHask.Sym.Monomial
    ( m
    , mulM
    ) where

import qualified Data.Map.Strict as Map

import QHask.Partition (Partition, partitionsOf, weight)
import QHask.Sym.Types
import QHask.Sym.Kostka (kostkaNumber, inverseKostkaEntry)
import QHask.Sym.Schur (mulS)

-- | Monomial basis element m_μ.
m :: Partition -> SymFun Mono
m mu = SymFun (Map.singleton mu 1)

-- | Multiply monomial symmetric functions (via Schur).
mulM :: SymFun Mono -> SymFun Mono -> SymFun Mono
mulM a b = fromSchur (mulS (toSchur a) (toSchur b))

instance Basis Mono where
    basisName _ = "m"
    -- s_λ = Σ_μ K_{λ,μ} m_μ (definition of Kostka numbers)
    -- So m_μ = Σ_λ (K^{-1})_{μ,λ} s_λ  (transpose of inverse Kostka)
    toSchur (SymFun coeffs) = SymFun $ Map.filter (/= 0) $
        Map.fromListWith (+)
            [ (lam, c * inverseKostkaEntry mu lam)
            | (mu, c) <- Map.toList coeffs
            , lam <- partitionsOf (weight mu)
            ]
    -- s_λ = Σ_μ K_{λ,μ} m_μ
    fromSchur (SymFun coeffs) = SymFun $ Map.filter (/= 0) $
        Map.fromListWith (+)
            [ (mu, c * fromIntegral (kostkaNumber lam mu))
            | (lam, c) <- Map.toList coeffs
            , mu <- partitionsOf (weight lam)
            ]
