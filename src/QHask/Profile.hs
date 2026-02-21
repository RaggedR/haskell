module QHask.Profile
    ( Profile(..)
    , profileD
    , profileToMu
    , allProfiles
    ) where

-- | A rank-3 cylindric partition profile (c0, c1, c2).
data Profile = Profile
    { profC0 :: !Int
    , profC1 :: !Int
    , profC2 :: !Int
    } deriving (Eq, Ord, Show)

-- | Total weight d = c0 + c1 + c2.
profileD :: Profile -> Int
profileD (Profile c0 c1 c2) = c0 + c1 + c2

-- | Compute mu(c) = (c1+c2, c2, 0) for rank-3 profiles.
profileToMu :: Profile -> (Int, Int, Int)
profileToMu (Profile _ c1 c2) = (c1 + c2, c2, 0)

-- | All profiles with given total weight d.
-- These are compositions of d into 3 non-negative parts.
allProfiles :: Int -> [Profile]
allProfiles d =
    [ Profile c0 c1 c2
    | c0 <- [0..d]
    , c1 <- [0..d - c0]
    , let c2 = d - c0 - c1
    ]
