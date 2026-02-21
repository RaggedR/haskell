module QHask.CPPSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map

import QHask.CPP
import QHask.Profile

spec :: Spec
spec = do
    describe "computeGK" $ do
        it "d=0, c=(0,0,0): only the empty CPP" $
            let gk = computeGK (Profile 0 0 0) 5 5
            in Map.lookup (0, 0) gk `shouldBe` Just 1

        it "d=1, c=(1,0,0) at z=1: matches partition numbers" $
            -- GK_{(1,0,0)}(1,q) = 1/(q;q)_inf, so coefficients are p(n)
            let gk = computeGK (Profile 1 0 0) 10 10
                z1 = gkAtZ1 gk 10
            -- p(0)=1, p(1)=1, p(2)=2, p(3)=3, p(4)=5, p(5)=7
            in take 6 z1 `shouldBe` [1, 1, 2, 3, 5, 7]

    describe "extractZCoeff" $ do
        it "extracts correct z^0 coefficient (empty partitions only)" $
            let gk = computeGK (Profile 1 0 0) 5 5
                g0 = extractZCoeff gk 0
            -- [z^0]: only the empty CPP contributes, so g_0(q) = 1
            in Map.lookup 0 g0 `shouldBe` Just 1

    describe "gkAtZ1" $ do
        it "total count at weight 0 is 1 (empty CPP)" $
            let gk = computeGK (Profile 2 1 1) 5 5
                z1 = gkAtZ1 gk 5
            in head z1 `shouldBe` 1
