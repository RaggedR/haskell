{-# OPTIONS_GHC -Wno-orphans #-}
module QHask.Sym.Schur
    ( s
    , mulS
    ) where

import qualified Data.Map.Strict as Map

import QHask.Partition (Partition)
import QHask.Sym.Types
import QHask.Sym.LR (lrRule)

-- | Schur basis element s_λ.
s :: Partition -> SymFun Schur
s lam = SymFun (Map.singleton lam 1)

-- | Multiply two Schur functions using the Littlewood-Richardson rule.
-- s_λ · s_μ = Σ c^ν_{λ,μ} s_ν
mulS :: SymFun Schur -> SymFun Schur -> SymFun Schur
mulS (SymFun a) (SymFun b) = SymFun $ Map.filter (/= 0) $
    Map.fromListWith (+)
        [ (nu, ca * cb * fromIntegral cnu)
        | (lam, ca) <- Map.toList a
        , (mu, cb)  <- Map.toList b
        , (nu, cnu) <- Map.toList (lrRule lam mu)
        ]

instance Basis Schur where
    basisName _ = "s"
    toSchur     = id
    fromSchur   = id
