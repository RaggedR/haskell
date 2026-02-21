module QHask.ProfileSpec (spec) where

import Test.Hspec
import QHask.Profile

spec :: Spec
spec = do
    describe "profileD" $ do
        it "sums components" $
            profileD (Profile 2 1 1) `shouldBe` 4
        it "d=0 for zero profile" $
            profileD (Profile 0 0 0) `shouldBe` 0

    describe "profileToMu" $ do
        it "mu(2,1,1) = (2,1,0)" $
            profileToMu (Profile 2 1 1) `shouldBe` (2, 1, 0)
        it "mu(1,0,0) = (0,0,0)" $
            profileToMu (Profile 1 0 0) `shouldBe` (0, 0, 0)
        it "mu(0,0,3) = (3,3,0)" $
            profileToMu (Profile 0 0 3) `shouldBe` (3, 3, 0)

    describe "allProfiles" $ do
        it "d=0 has 1 profile" $
            length (allProfiles 0) `shouldBe` 1
        it "d=1 has 3 profiles" $
            length (allProfiles 1) `shouldBe` 3
        it "d=4 has 15 profiles (C(6,2))" $
            length (allProfiles 4) `shouldBe` 15
        it "all profiles sum to d" $
            all (\p -> profileD p == 4) (allProfiles 4) `shouldBe` True
