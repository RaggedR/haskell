module QHask.Sym.KostkaSpec (spec) where

import Test.Hspec
import QHask.Partition
import QHask.Sym.Kostka

spec :: Spec
spec = do
    describe "kostkaNumber" $ do
        it "K_{(n),(1^n)} = 1 (single column tableau)" $
            kostkaNumber (Partition [3]) (Partition [1,1,1]) `shouldBe` 1
        it "K_{(1^n),(1^n)} = 1 (single row tableau)" $
            kostkaNumber (Partition [1,1,1]) (Partition [1,1,1]) `shouldBe` 1
        it "K_{(3),(3)} = 1" $
            kostkaNumber (Partition [3]) (Partition [3]) `shouldBe` 1
        it "K_{(2,1),(1,1,1)} = 2 (classic)" $
            kostkaNumber (Partition [2,1]) (Partition [1,1,1]) `shouldBe` 2
        it "K_{(3),(2,1)} = 1" $
            kostkaNumber (Partition [3]) (Partition [2,1]) `shouldBe` 1
        it "K_{(2,1),(2,1)} = 1" $
            kostkaNumber (Partition [2,1]) (Partition [2,1]) `shouldBe` 1
        it "K_{λ,μ} = 0 when λ does not dominate μ" $
            kostkaNumber (Partition [1,1,1]) (Partition [3]) `shouldBe` 0
        it "K_{(3,1),(2,2)} = 1" $
            kostkaNumber (Partition [3,1]) (Partition [2,2]) `shouldBe` 1
        it "K_{(2,2),(2,2)} = 1" $
            kostkaNumber (Partition [2,2]) (Partition [2,2]) `shouldBe` 1
        it "K_{(4),(1,1,1,1)} = 1" $
            kostkaNumber (Partition [4]) (Partition [1,1,1,1]) `shouldBe` 1
        it "K_{(3,1),(1,1,1,1)} = 3" $
            kostkaNumber (Partition [3,1]) (Partition [1,1,1,1]) `shouldBe` 3
        it "K_{(2,2),(1,1,1,1)} = 2" $
            kostkaNumber (Partition [2,2]) (Partition [1,1,1,1]) `shouldBe` 2

    describe "enumerateSSYT" $ do
        it "single SSYT of shape (2,1) content (2,1)" $ do
            let tabs = enumerateSSYT (Partition [2,1]) (Partition [2,1])
            length tabs `shouldBe` 1
            head tabs `shouldBe` [[1,1],[2]]
        it "two SSYT of shape (2,1) content (1,1,1)" $ do
            let tabs = enumerateSSYT (Partition [2,1]) (Partition [1,1,1])
            length tabs `shouldBe` 2

    describe "inverseKostkaEntry" $ do
        it "diagonal entries are 1" $
            inverseKostkaEntry (Partition [3]) (Partition [3]) `shouldBe` 1
        it "(K^{-1})_{(3),(2,1)} = -1" $
            inverseKostkaEntry (Partition [3]) (Partition [2,1]) `shouldBe` (-1)
        it "(K^{-1})_{(3),(1,1,1)} = 1" $
            inverseKostkaEntry (Partition [3]) (Partition [1,1,1]) `shouldBe` 1
        it "(K^{-1})_{(2,1),(1,1,1)} = -2" $
            inverseKostkaEntry (Partition [2,1]) (Partition [1,1,1]) `shouldBe` (-2)
        -- Verify K · K^{-1} = I for n=3
        it "K · K^{-1} = I for n=3" $ do
            let parts = partitionsOf 3
            and [ sum [fromIntegral (kostkaNumber lam nu) * inverseKostkaEntry nu mu
                      | nu <- parts]
                  == (if lam == mu then 1 else 0)
                | lam <- parts, mu <- parts
                ] `shouldBe` True
        -- Verify K · K^{-1} = I for n=4
        it "K · K^{-1} = I for n=4" $ do
            let parts = partitionsOf 4
            and [ abs (sum [fromIntegral (kostkaNumber lam nu) * inverseKostkaEntry nu mu
                           | nu <- parts]
                       - (if lam == mu then 1 else 0)) < 1e-10
                | lam <- parts, mu <- parts
                ] `shouldBe` True
