-- | Symmetric functions library: 5 classical bases with full change-of-basis
-- and multiplication. All conversions route through the Schur basis.
module QHask.Sym
    ( -- * Re-exports
      module QHask.Sym.Types
    , module QHask.Sym.Schur
    , module QHask.Sym.Monomial
    , module QHask.Sym.Homogeneous
    , module QHask.Sym.Elementary
    , module QHask.Sym.PowerSum
    , module QHask.Sym.Kostka
    , module QHask.Sym.LR
      -- * Generic operations
    , changeBasis
    , mul
    , omega
    , innerProduct
    ) where

import qualified Data.Map.Strict as Map

import QHask.Partition (conjugate)
import QHask.Sym.Types
import QHask.Sym.Kostka
import QHask.Sym.LR
import QHask.Sym.Schur
import QHask.Sym.Monomial
import QHask.Sym.Homogeneous
import QHask.Sym.Elementary
import QHask.Sym.PowerSum

-- | Change basis: convert from any basis to any other, routing through Schur.
changeBasis :: (Basis a, Basis b) => SymFun a -> SymFun b
changeBasis = fromSchur . toSchur

-- | Generic multiplication: convert to Schur, multiply, convert back.
mul :: (Basis a) => SymFun a -> SymFun a -> SymFun a
mul a b = fromSchur (mulS (toSchur a) (toSchur b))

-- | The omega involution on Schur functions: ω(s_λ) = s_{λ'}.
omega :: SymFun Schur -> SymFun Schur
omega (SymFun coeffs) = SymFun $
    Map.mapKeys conjugate coeffs

-- | The Hall inner product: ⟨s_λ, s_μ⟩ = δ_{λ,μ}.
-- Both arguments must be in the Schur basis.
innerProduct :: SymFun Schur -> SymFun Schur -> Rational
innerProduct (SymFun a) (SymFun b) =
    sum [ ca * Map.findWithDefault 0 lam b
        | (lam, ca) <- Map.toList a
        ]
