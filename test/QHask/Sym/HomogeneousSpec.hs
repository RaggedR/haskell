module QHask.Sym.HomogeneousSpec (spec) where

import Test.Hspec

import QHask.Partition (Partition(..))
import QHask.Sym.Types
import QHask.Sym.Schur (s)
import QHask.Sym.Homogeneous

spec :: Spec
spec = do
    describe "mulH" $ do
        it "h_(2) * h_(1) = h_(2,1)" $
            mulH (h (Partition [2])) (h (Partition [1]))
                `shouldBe` h (Partition [2,1])

        it "h_(1) * h_(1) = h_(1,1)" $
            mulH (h (Partition [1])) (h (Partition [1]))
                `shouldBe` h (Partition [1,1])

        it "h_(2) * h_(2) = h_(2,2)" $
            mulH (h (Partition [2])) (h (Partition [2]))
                `shouldBe` h (Partition [2,2])

    describe "Basis Homo" $ do
        -- h_(2,1) = Σ_λ K_{λ,(2,1)} s_λ = s_(3) + s_(2,1)
        -- because K_{(3),(2,1)} = 1 and K_{(2,1),(2,1)} = 1
        it "h_(2,1) in Schur basis: s_(3) + s_(2,1)" $ do
            let result = toSchur (h (Partition [2,1]))
            coeffOf (Partition [3]) result `shouldBe` 1
            coeffOf (Partition [2,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 2

        -- h_n = s_{(n)} (complete homogeneous = single-row Schur)
        it "h_3 = s_(3)" $ do
            let result = toSchur (h (Partition [3]))
            result `shouldBe` s (Partition [3])

        it "round-trip h -> s -> h is identity" $ do
            let original = h (Partition [2,1])
                roundTrip = fromSchur (toSchur original) :: SymFun Homo
            roundTrip `shouldBe` original

        it "round-trip s -> h -> s is identity" $ do
            let original = s (Partition [2,1])
                asH = fromSchur original :: SymFun Homo
                roundTrip = toSchur asH
            roundTrip `shouldBe` original

        it "h_(1,1) in Schur: s_(2) + s_(1,1)" $ do
            let result = toSchur (h (Partition [1,1]))
            coeffOf (Partition [2]) result `shouldBe` 1
            coeffOf (Partition [1,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 2
