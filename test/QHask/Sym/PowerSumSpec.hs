module QHask.Sym.PowerSumSpec (spec) where

import Test.Hspec

import QHask.Partition (Partition(..))
import QHask.Sym.Types
import QHask.Sym.Schur (s)
import QHask.Sym.PowerSum

spec :: Spec
spec = do
    describe "zCoeff" $ do
        it "z_{(n)} = n" $
            zCoeff (Partition [3]) `shouldBe` 3
        it "z_{(1,1,1)} = 1^3 * 3! = 6" $
            zCoeff (Partition [1,1,1]) `shouldBe` 6
        it "z_{(2,1)} = 2^1 * 1! * 1^1 * 1! = 2" $
            zCoeff (Partition [2,1]) `shouldBe` 2
        it "z_{(2,2)} = 2^2 * 2! = 8" $
            zCoeff (Partition [2,2]) `shouldBe` 8
        it "z_{()} = 1" $
            zCoeff (Partition []) `shouldBe` 1

    describe "character" $ do
        -- Characters of S_3
        it "χ^{(3)}((1,1,1)) = 1 (trivial rep, identity)" $
            character (Partition [3]) (Partition [1,1,1]) `shouldBe` 1
        it "χ^{(1,1,1)}((1,1,1)) = 1 (sign rep, identity)" $
            character (Partition [1,1,1]) (Partition [1,1,1]) `shouldBe` 1
        it "χ^{(1,1,1)}((3)) = 1 (sign rep, 3-cycle)" $
            character (Partition [1,1,1]) (Partition [3]) `shouldBe` 1
        it "χ^{(3)}((3)) = 1 (trivial rep, 3-cycle)" $
            character (Partition [3]) (Partition [3]) `shouldBe` 1
        it "χ^{(2,1)}((1,1,1)) = 2 (standard rep, identity)" $
            character (Partition [2,1]) (Partition [1,1,1]) `shouldBe` 2
        it "χ^{(2,1)}((3)) = -1 (standard rep, 3-cycle)" $
            character (Partition [2,1]) (Partition [3]) `shouldBe` (-1)
        it "χ^{(2,1)}((2,1)) = 0 (standard rep, transposition)" $
            character (Partition [2,1]) (Partition [2,1]) `shouldBe` 0

    describe "Basis Pow" $ do
        -- p_1 = s_1
        it "p_(1) = s_(1)" $
            toSchur (p (Partition [1])) `shouldBe` s (Partition [1])

        -- p_(1,1) = s_(2) + s_(1,1)  (= h_2)
        it "p_(1,1) = s_(2) + s_(1,1)" $ do
            let result = toSchur (p (Partition [1,1]))
            coeffOf (Partition [2]) result `shouldBe` 1
            coeffOf (Partition [1,1]) result `shouldBe` 1

        -- p_2 = s_(2) - s_(1,1)
        it "p_(2) = s_(2) - s_(1,1)" $ do
            let result = toSchur (p (Partition [2]))
            coeffOf (Partition [2]) result `shouldBe` 1
            coeffOf (Partition [1,1]) result `shouldBe` (-1)

        it "round-trip p -> s -> p is identity" $ do
            let original = p (Partition [2,1])
                roundTrip = fromSchur (toSchur original) :: SymFun Pow
            roundTrip `shouldBe` original

        -- p_3 = s_(3) - s_(2,1) + s_(1,1,1) (from character table of S_3)
        it "p_(3) = s_(3) - s_(2,1) + s_(1,1,1)" $ do
            let result = toSchur (p (Partition [3]))
            coeffOf (Partition [3]) result `shouldBe` 1
            coeffOf (Partition [2,1]) result `shouldBe` (-1)
            coeffOf (Partition [1,1,1]) result `shouldBe` 1
