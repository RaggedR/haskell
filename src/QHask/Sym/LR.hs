module QHask.Sym.LR
    ( lrRule
    , lrCoeff
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import QHask.Partition

-- | Compute all LR coefficients c^ν_{λ,μ} for the product s_λ · s_μ.
-- Returns a map from ν to c^ν_{λ,μ}.
--
-- TODO: Currently generates all partitionsOf n and filters by containedIn.
-- Could be optimized to only generate candidate ν with λ ⊆ ν directly.
lrRule :: Partition -> Partition -> Map Partition Integer
lrRule lam mu
    | weight lam == 0 = Map.singleton mu 1
    | weight mu == 0  = Map.singleton lam 1
    | otherwise       = Map.fromListWith (+)
        [ (nu, 1)
        | nu <- partitionsOf n
        , lam `containedIn` nu
        , _ <- lrTableaux nu lam mu
        ]
  where
    n = weight lam + weight mu

-- | Single LR coefficient c^ν_{λ,μ}.
lrCoeff :: Partition -> Partition -> Partition -> Integer
lrCoeff nu lam mu = Map.findWithDefault 0 nu (lrRule lam mu)

-- | Enumerate LR tableaux: skew SSYT of shape ν/λ with content μ
-- whose reverse reading word is a lattice word.
--
-- We build a chain λ = ν_0 ⊂ ν_1 ⊂ ... ⊂ ν_r = ν where each ν_k / ν_{k-1}
-- is a horizontal strip of size μ_k. The lattice word condition is checked
-- incrementally: when adding value k ≥ 2, for each row j,
-- Σ_{i≤j} (ν_k[i] - ν_{k-1}[i]) ≤ Σ_{i<j} (ν_{k-1}[i] - ν_{k-2}[i]).
lrTableaux :: Partition -> Partition -> Partition -> [()]
lrTableaux nu lam mu
    | weight nu /= weight lam + weight mu = []
    | not (lam `containedIn` nu)          = []
    | otherwise = buildChain (getParts mu) 1 lam lam
  where
    buildChain :: [Int] -> Int -> Partition -> Partition -> [()]
    buildChain [] _ inner _
        | inner == nu = [()]
        | otherwise   = []
    buildChain (count:rest) val inner prevInner =
        [ result
        | newInner <- horizontalStrips nu inner count
        , val == 1 || latticeCheck prevInner inner newInner
        , result <- buildChain rest (val + 1) newInner inner
        ]

-- | Check the incremental lattice condition when placing value k.
-- prevInner = ν_{k-2}, inner = ν_{k-1}, new = ν_k.
-- Condition: for all j, Σ_{i≤j} a_i ≤ Σ_{i<j} b_i
-- where a_i = new[i] - inner[i] and b_i = inner[i] - prev[i].
latticeCheck :: Partition -> Partition -> Partition -> Bool
latticeCheck prev inner new =
    let nRows = maximum [partLen new, partLen inner, partLen prev]
        a = [getPart new i - getPart inner i | i <- [0..nRows-1]]
        b = [getPart inner i - getPart prev i | i <- [0..nRows-1]]
        cumA = scanl1 (+) a
        cumBShifted = scanl (+) 0 b  -- [0, b_0, b_0+b_1, ...]
    in all (uncurry (<=)) (zip cumA cumBShifted)

