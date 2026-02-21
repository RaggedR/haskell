module QHask.ConjectureSpec (spec) where

import Test.Hspec
import QHask.Conjecture
import QHask.Profile

spec :: Spec
spec = do
    describe "computeQ" $ do
        it "Q_{0,c}(q) = 1 for any profile" $
            head (computeQ (Profile 1 0 0) 0 5) `shouldBe` 1

        it "d=2, c=(2,0,0): Q_1 = q^2 (second RR family, Q_m = q^{m(m+1)})" $
            -- mu=(0,0,0), giving the second RR identity
            let q1 = computeQ (Profile 2 0 0) 1 5
            in take 4 q1 `shouldBe` [0, 0, 1, 0]

        it "d=2, c=(2,0,0): Q_2 = q^6" $
            let q2 = computeQ (Profile 2 0 0) 2 10
            in take 8 q2 `shouldBe` [0, 0, 0, 0, 0, 0, 1, 0]

        it "d=2, c=(1,1,0): Q_1 = q (first RR family, Q_m = q^{m^2})" $
            -- mu=(1,0,0), giving the first RR identity
            let q1 = computeQ (Profile 1 1 0) 1 5
            in take 3 q1 `shouldBe` [0, 1, 0]

    describe "verifyPositivity" $ do
        it "d=1: Q_{n,(1,0,0)} nonneg for n=0..2" $
            verifyPositivity (Profile 1 0 0) 2 10 `shouldBe` True

        it "d=2: Q_{n,(2,0,0)} nonneg for n=0..2" $
            verifyPositivity (Profile 2 0 0) 2 10 `shouldBe` True

        it "d=2: Q_{n,(1,1,0)} nonneg for n=0..2" $
            verifyPositivity (Profile 1 1 0) 2 10 `shouldBe` True
