{-# OPTIONS_GHC -Wno-orphans #-}
module QHask.Sym.Elementary
    ( e
    , mulE
    ) where

import Data.List (sortBy)
import qualified Data.Map.Strict as Map

import QHask.Partition (Partition(..), conjugate, partitionsOf, weight)
import QHask.Sym.Types
import QHask.Sym.Kostka (kostkaNumber, inverseKostkaEntry)

-- | Elementary symmetric function basis element e_μ.
e :: Partition -> SymFun Elem
e mu = SymFun (Map.singleton mu 1)

-- | Multiply elementary symmetric functions: e_μ · e_ν = e_{μ∪ν}.
mulE :: SymFun Elem -> SymFun Elem -> SymFun Elem
mulE (SymFun a) (SymFun b) = SymFun $ Map.filter (/= 0) $
    Map.fromListWith (+)
        [ (mergeParts mu nu, ca * cb)
        | (mu, ca) <- Map.toList a
        , (nu, cb) <- Map.toList b
        ]

mergeParts :: Partition -> Partition -> Partition
mergeParts (Partition ps) (Partition qs) =
    Partition $ sortBy (flip compare) (ps ++ qs)

instance Basis Elem where
    basisName _ = "e"
    -- e_μ = ω(h_μ), and ω(s_λ) = s_{λ'}.
    -- Since h_μ = Σ_λ K_{λ,μ} s_λ, we get e_μ = Σ_λ K_{λ,μ} s_{λ'} = Σ_ν K_{ν',μ} s_ν.
    toSchur (SymFun coeffs) = SymFun $ Map.filter (/= 0) $
        Map.fromListWith (+)
            [ (lam, c * fromIntegral (kostkaNumber (conjugate lam) mu))
            | (mu, c) <- Map.toList coeffs
            , lam <- partitionsOf (weight mu)
            ]
    -- s → e: b_μ = Σ_λ (K^{-1})_{μ,λ'} a_λ
    fromSchur (SymFun coeffs) = SymFun $ Map.filter (/= 0) $
        Map.fromListWith (+)
            [ (mu, c * inverseKostkaEntry mu (conjugate lam))
            | (lam, c) <- Map.toList coeffs
            , mu <- partitionsOf (weight lam)
            ]
