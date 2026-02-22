module QHask.Sym.SchurSpec (spec) where

import Test.Hspec

import QHask.Partition (Partition(..))
import QHask.Sym.Types
import QHask.Sym.Schur

spec :: Spec
spec = do
    describe "s" $ do
        it "creates a single basis element" $
            coeffOf (Partition [2,1]) (s (Partition [2,1])) `shouldBe` 1
        it "other coefficients are 0" $
            coeffOf (Partition [3]) (s (Partition [2,1])) `shouldBe` 0

    describe "mulS" $ do
        it "s_(1) * s_(1) = s_(2) + s_(1,1)" $ do
            let result = mulS (s (Partition [1])) (s (Partition [1]))
            coeffOf (Partition [2]) result `shouldBe` 1
            coeffOf (Partition [1,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 2

        it "s_(2) * s_(1) = s_(3) + s_(2,1)" $ do
            let result = mulS (s (Partition [2])) (s (Partition [1]))
            coeffOf (Partition [3]) result `shouldBe` 1
            coeffOf (Partition [2,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 2

        it "s_(1,1) * s_(1,1) = s_(2,2) + s_(2,1,1) + s_(1,1,1,1)" $ do
            let result = mulS (s (Partition [1,1])) (s (Partition [1,1]))
            coeffOf (Partition [2,2]) result `shouldBe` 1
            coeffOf (Partition [2,1,1]) result `shouldBe` 1
            coeffOf (Partition [1,1,1,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 3

        it "multiplication is commutative" $ do
            let a = mulS (s (Partition [2,1])) (s (Partition [2]))
                b = mulS (s (Partition [2])) (s (Partition [2,1]))
            a `shouldBe` b

        it "multiplication is associative" $ do
            let s1 = s (Partition [1])
                a = mulS (mulS s1 s1) s1
                b = mulS s1 (mulS s1 s1)
            a `shouldBe` b

        it "s * 0 = 0" $
            mulS (s (Partition [2,1])) zero `shouldBe` zero

        it "multiplying by s_() is identity" $
            mulS (s (Partition [2,1])) (s (Partition [])) `shouldBe` s (Partition [2,1])
