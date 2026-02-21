module QHask.QSeriesSpec (spec) where

import Test.Hspec
import QHask.QSeries

spec :: Spec
spec = do
    describe "oneQS" $ do
        it "has leading coefficient 1" $
            take 5 (coeffs oneQS) `shouldBe` [1, 0, 0, 0, 0]

    describe "qPower" $ do
        it "q^0 = 1" $
            take 4 (coeffs (qPower 0)) `shouldBe` [1, 0, 0, 0]
        it "q^3 has a 1 at index 3" $
            take 5 (coeffs (qPower 3)) `shouldBe` [0, 0, 0, 1, 0]

    describe "addQS / subQS" $ do
        it "1 + q = [1,1,0,...]" $
            take 4 (coeffs (addQS oneQS (qPower 1))) `shouldBe` [1, 1, 0, 0]
        it "1 - 1 = 0" $
            take 4 (coeffs (subQS oneQS oneQS)) `shouldBe` [0, 0, 0, 0]

    describe "mulQS" $ do
        it "1 * f = f" $
            take 6 (coeffs (mulQS oneQS (qFactorial 3))) `shouldBe`
            take 6 (coeffs (qFactorial 3))
        it "q * q = q^2" $
            take 5 (coeffs (mulQS (qPower 1) (qPower 1))) `shouldBe` [0, 0, 1, 0, 0]
        it "(1+q)(1-q) = 1-q^2" $
            let onePlusQ  = addQS oneQS (qPower 1)
                oneMinusQ = subQS oneQS (qPower 1)
            in take 5 (coeffs (mulQS onePlusQ oneMinusQ)) `shouldBe` [1, 0, -1, 0, 0]

    describe "qFactorial" $ do
        it "(q)_0 = 1" $
            take 4 (coeffs (qFactorial 0)) `shouldBe` [1, 0, 0, 0]
        it "(q)_1 = 1 - q" $
            take 4 (coeffs (qFactorial 1)) `shouldBe` [1, -1, 0, 0]
        it "(q)_2 = (1-q)(1-q^2) = 1 - q - q^2 + q^3" $
            take 5 (coeffs (qFactorial 2)) `shouldBe` [1, -1, -1, 1, 0]
        it "(q)_3 first 7 coeffs" $
            -- (1-q)(1-q^2)(1-q^3) = 1 - q - q^2 + q^5 + q^4 - ... wait let me compute:
            -- Actually: expand step by step
            -- (1-q)(1-q^2) = 1 - q - q^2 + q^3
            -- * (1-q^3) = 1 - q - q^2 + q^3 - q^3 + q^4 + q^5 - q^6
            --           = 1 - q - q^2 + 0 + q^4 + q^5 - q^6
            take 7 (coeffs (qFactorial 3)) `shouldBe` [1, -1, -1, 0, 1, 1, -1]

    describe "inverseQS" $ do
        it "1/(1-q) = 1 + q + q^2 + ..." $
            let oneMinusQ = subQS oneQS (qPower 1)
            in take 6 (coeffs (inverseQS oneMinusQ)) `shouldBe` [1, 1, 1, 1, 1, 1]
        it "1/(q)_3 first 7 coeffs = partitions into parts <= 3" $
            -- p(n, parts<=3): 1, 1, 2, 3, 4, 5, 7
            take 7 (coeffs (inverseQS (qFactorial 3))) `shouldBe` [1, 1, 2, 3, 4, 5, 7]
        it "f * (1/f) = 1 for (q)_2" $
            let f = qFactorial 2
                g = inverseQS f
            in take 8 (coeffs (mulQS f g)) `shouldBe` [1, 0, 0, 0, 0, 0, 0, 0]

    describe "qBinomial" $ do
        it "[n choose 0]_q = 1" $
            take 3 (coeffs (qBinomial 5 0)) `shouldBe` [1, 0, 0]
        it "[n choose n]_q = 1" $
            take 3 (coeffs (qBinomial 5 5)) `shouldBe` [1, 0, 0]
        it "[4 choose 2]_q = 1 + q + 2q^2 + q^3 + q^4" $
            take 6 (coeffs (qBinomial 4 2)) `shouldBe` [1, 1, 2, 1, 1, 0]
        it "[3 choose 1]_q = 1 + q + q^2" $
            take 4 (coeffs (qBinomial 3 1)) `shouldBe` [1, 1, 1, 0]

    describe "qPochhammer" $ do
        it "(q;q)_inf first 10 coeffs (Euler function, OEIS A010815)" $
            -- prod_{k>=1} (1-q^k) = 1 - q - q^2 + q^5 + q^7 - q^12 - q^15 + ...
            -- First 10: [1, -1, -1, 0, 0, 1, 0, 1, 0, 0]
            take 10 (coeffs (qPochhammer 1 1 20)) `shouldBe`
                [1, -1, -1, 0, 0, 1, 0, 1, 0, 0]
        it "(q;q)_inf first 15 coeffs (extended Euler)" $
            take 15 (coeffs (qPochhammer 1 1 20)) `shouldBe`
                [1, -1, -1, 0, 0, 1, 0, 1, 0, 0, 0, 0, -1, 0, 0]
