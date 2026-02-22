module QHask.Sym.Types
    ( -- * Phantom basis tags
      Mono, Elem, Homo, Pow, Schur
      -- * Symmetric function type
    , SymFun(..)
      -- * Basis class
    , Basis(..)
      -- * Arithmetic
    , zero, add, sub, neg, scale
      -- * Queries
    , degree, isHomogeneous, coeffOf, terms, mapCoeffs
      -- * Display
    , showSym
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio (numerator, denominator)
import Data.List (intercalate, sortBy)
import Data.Ord (Down(..))

import QHask.Partition (Partition(..), weight)

-- | Phantom types for each basis.
data Mono
data Elem
data Homo
data Pow
data Schur

-- | A symmetric function in basis @b@, represented as a linear combination
-- of basis elements indexed by partitions with rational coefficients.
newtype SymFun b = SymFun { getCoeffs :: Map Partition Rational }
    deriving (Eq, Show)

-- | Class for bases that can convert to/from the Schur basis.
class Basis b where
    basisName :: proxy b -> String
    toSchur   :: SymFun b -> SymFun Schur
    fromSchur :: SymFun Schur -> SymFun b

-- | The zero symmetric function.
zero :: SymFun b
zero = SymFun Map.empty

-- | Add two symmetric functions in the same basis.
add :: SymFun b -> SymFun b -> SymFun b
add (SymFun a) (SymFun b) = SymFun $ Map.filter (/= 0) $ Map.unionWith (+) a b

-- | Subtract.
sub :: SymFun b -> SymFun b -> SymFun b
sub (SymFun a) (SymFun b) = SymFun $ Map.filter (/= 0) $ Map.unionWith (+) a (Map.map negate b)

-- | Negate.
neg :: SymFun b -> SymFun b
neg (SymFun a) = SymFun $ Map.map negate a

-- | Scale by a rational constant.
scale :: Rational -> SymFun b -> SymFun b
scale 0 _ = zero
scale c (SymFun a) = SymFun $ Map.filter (/= 0) $ Map.map (* c) a

-- | The degree (maximum weight of any partition with nonzero coefficient).
-- Returns 0 for the zero function.
degree :: SymFun b -> Int
degree (SymFun m)
    | Map.null m = 0
    | otherwise  = maximum $ map weight $ Map.keys m

-- | Is this function homogeneous of some degree?
isHomogeneous :: SymFun b -> Bool
isHomogeneous (SymFun m)
    | Map.null m = True
    | otherwise  = let ws = map weight (Map.keys m)
                   in all (== head ws) ws

-- | Coefficient of a particular partition.
coeffOf :: Partition -> SymFun b -> Rational
coeffOf p (SymFun m) = Map.findWithDefault 0 p m

-- | List of (partition, coefficient) pairs, sorted by partition.
terms :: SymFun b -> [(Partition, Rational)]
terms (SymFun m) = Map.toAscList m

-- | Map a function over all coefficients, removing zeros.
mapCoeffs :: (Rational -> Rational) -> SymFun b -> SymFun b
mapCoeffs f (SymFun m) = SymFun $ Map.filter (/= 0) $ Map.map f m

-- | Pretty-print a symmetric function with a given basis prefix (e.g. "s", "m").
showSym :: String -> SymFun b -> String
showSym prefix (SymFun m)
    | Map.null m = "0"
    | otherwise  = intercalate " + " $ map showTerm sorted
  where
    sorted = sortBy (\(a,_) (b,_) -> compare (Down $ weight a, a) (Down $ weight b, b))
           $ Map.toList m
    showTerm (Partition ps, c)
        | c == 1    = prefix ++ showParts ps
        | c == -1   = "-" ++ prefix ++ showParts ps
        | otherwise = showRat c ++ "*" ++ prefix ++ showParts ps
    showParts [] = "_()"
    showParts ps = "_(" ++ intercalate "," (map show ps) ++ ")"
    showRat r
        | denominator r == 1 = show (numerator r)
        | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)
