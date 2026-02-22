module QHask.Sym.LRSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map

import QHask.Partition
import QHask.Sym.LR

spec :: Spec
spec = do
    describe "lrRule" $ do
        it "s_(1) * s_(1) = s_(2) + s_(1,1)" $
            lrRule (Partition [1]) (Partition [1])
                `shouldBe` Map.fromList [(Partition [2], 1), (Partition [1,1], 1)]

        it "s_(2) * s_(1) = s_(3) + s_(2,1) (Pieri rule)" $
            lrRule (Partition [2]) (Partition [1])
                `shouldBe` Map.fromList [(Partition [3], 1), (Partition [2,1], 1)]

        it "s_(1,1) * s_(1) = s_(2,1) + s_(1,1,1) (Pieri rule)" $
            lrRule (Partition [1,1]) (Partition [1])
                `shouldBe` Map.fromList [(Partition [2,1], 1), (Partition [1,1,1], 1)]

        it "s_(2) * s_(2) = s_(4) + s_(3,1) + s_(2,2)" $
            lrRule (Partition [2]) (Partition [2])
                `shouldBe` Map.fromList [(Partition [4], 1), (Partition [3,1], 1), (Partition [2,2], 1)]

        it "s_(2) * s_(1,1) = s_(3,1) + s_(2,1,1)" $
            lrRule (Partition [2]) (Partition [1,1])
                `shouldBe` Map.fromList [(Partition [3,1], 1), (Partition [2,1,1], 1)]

        it "s_(2,1) * s_(1) = s_(3,1) + s_(2,2) + s_(2,1,1)" $
            lrRule (Partition [2,1]) (Partition [1])
                `shouldBe` Map.fromList [(Partition [3,1], 1), (Partition [2,2], 1), (Partition [2,1,1], 1)]

        -- Commutativity
        it "LR is commutative: s_(2,1) * s_(2) = s_(2) * s_(2,1)" $
            lrRule (Partition [2,1]) (Partition [2])
                `shouldBe` lrRule (Partition [2]) (Partition [2,1])

    describe "lrCoeff" $ do
        it "c^{(3,1)}_{(2),(1,1)} = 1" $
            lrCoeff (Partition [3,1]) (Partition [2]) (Partition [1,1]) `shouldBe` 1
        it "c^{(4)}_{(2),(1,1)} = 0" $
            lrCoeff (Partition [4]) (Partition [2]) (Partition [1,1]) `shouldBe` 0
