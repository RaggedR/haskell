module QHask.Sym.ElementarySpec (spec) where

import Test.Hspec

import QHask.Partition (Partition(..))
import QHask.Sym.Types
import QHask.Sym.Schur (s)
import QHask.Sym.Elementary

spec :: Spec
spec = do
    describe "mulE" $ do
        it "e_(1,1) * e_(1) = e_(1,1,1)" $
            mulE (e (Partition [1,1])) (e (Partition [1]))
                `shouldBe` e (Partition [1,1,1])

    describe "Basis Elem" $ do
        -- e_n = s_{(1^n)} (single-column Schur)
        it "e_1 = s_(1)" $ do
            let result = toSchur (e (Partition [1]))
            result `shouldBe` s (Partition [1])

        it "e_2 = s_(1,1)" $ do
            let result = toSchur (e (Partition [2]))
            result `shouldBe` s (Partition [1,1])

        it "e_3 = s_(1,1,1)" $ do
            let result = toSchur (e (Partition [3]))
            result `shouldBe` s (Partition [1,1,1])

        -- e_(2,1) = omega(h_(2,1)) = omega(s_(3) + s_(2,1)) = s_(1,1,1) + s_(2,1)
        it "e_(2,1) in Schur: s_(1,1,1) + s_(2,1)" $ do
            let result = toSchur (e (Partition [2,1]))
            coeffOf (Partition [1,1,1]) result `shouldBe` 1
            coeffOf (Partition [2,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 2

        it "round-trip e -> s -> e is identity" $ do
            let original = e (Partition [2,1])
                roundTrip = fromSchur (toSchur original) :: SymFun Elem
            roundTrip `shouldBe` original

        it "round-trip s -> e -> s is identity" $ do
            let original = s (Partition [2,1])
                asE = fromSchur original :: SymFun Elem
                roundTrip = toSchur asE
            roundTrip `shouldBe` original
