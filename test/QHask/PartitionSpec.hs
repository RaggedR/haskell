module QHask.PartitionSpec (spec) where

import Test.Hspec
import QHask.Partition

spec :: Spec
spec = do
    describe "mkPartition" $ do
        it "accepts valid partitions" $
            mkPartition [3, 2, 1] `shouldBe` Just (Partition [3, 2, 1])
        it "accepts empty partition" $
            mkPartition [] `shouldBe` Just emptyPartition
        it "accepts equal parts" $
            mkPartition [3, 3, 2] `shouldBe` Just (Partition [3, 3, 2])
        it "rejects non-decreasing" $
            mkPartition [1, 3] `shouldBe` Nothing
        it "rejects zero parts" $
            mkPartition [3, 0, 1] `shouldBe` Nothing
        it "rejects negative parts" $
            mkPartition [3, -1] `shouldBe` Nothing

    describe "getPart" $ do
        it "returns parts at valid indices" $
            getPart (Partition [5, 3, 1]) 1 `shouldBe` 3
        it "returns 0 for out-of-bounds" $
            getPart (Partition [5, 3, 1]) 4 `shouldBe` 0
        it "returns 0 for empty partition" $
            getPart emptyPartition 0 `shouldBe` 0

    describe "weight" $ do
        it "sums all parts" $
            weight (Partition [3, 2, 1]) `shouldBe` 6
        it "empty partition has weight 0" $
            weight emptyPartition `shouldBe` 0

    describe "partLen" $ do
        it "counts parts" $
            partLen (Partition [3, 2, 1]) `shouldBe` 3
        it "empty partition has length 0" $
            partLen emptyPartition `shouldBe` 0

    describe "conjugate" $ do
        it "conjugate [4,2,1] = [3,2,1,1]" $
            conjugate (Partition [4, 2, 1]) `shouldBe` Partition [3, 2, 1, 1]
        it "conjugate [3,3,3] = [3,3,3]" $
            conjugate (Partition [3, 3, 3]) `shouldBe` Partition [3, 3, 3]
        it "conjugate of empty is empty" $
            conjugate emptyPartition `shouldBe` emptyPartition
        it "conjugate is an involution" $
            let p = Partition [5, 3, 2, 1, 1]
            in conjugate (conjugate p) `shouldBe` p

    describe "partitionsOf" $ do
        it "p(0) = 1 (empty partition)" $
            length (partitionsOf 0) `shouldBe` 1
        it "p(1) = 1" $
            length (partitionsOf 1) `shouldBe` 1
        it "p(5) = 7 (OEIS A000041)" $
            length (partitionsOf 5) `shouldBe` 7
        it "p(10) = 42" $
            length (partitionsOf 10) `shouldBe` 42
        it "all partitions of 5 have weight 5" $
            all (\p -> weight p == 5) (partitionsOf 5) `shouldBe` True

    describe "genPartitions" $ do
        it "includes empty partition" $
            emptyPartition `elem` genPartitions 3 6 `shouldBe` True
        it "all results have parts <= maxPart" $
            all (\(Partition ps) -> all (<= 3) ps) (genPartitions 3 10) `shouldBe` True
        it "all results have weight <= maxSize" $
            all (\p -> weight p <= 10) (genPartitions 5 10) `shouldBe` True

    describe "genCompatible" $ do
        it "with shift=0, all parts dominated" $
            let prev = Partition [3, 2]
                compat = genCompatible prev 0 5 10
            in all (\p -> all (\j -> getPart p j <= getPart prev j)
                              [0..partLen p - 1]) compat
               `shouldBe` True
