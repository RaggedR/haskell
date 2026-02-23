module QHask.Sym.MonomialSpec (spec) where

import Test.Hspec

import QHask.Partition (Partition(..))
import QHask.Sym.Types
import QHask.Sym.Schur (s)
import QHask.Sym.Monomial

spec :: Spec
spec = do
    describe "Basis Mono" $ do
        -- s_λ = Σ_μ K_{λ,μ} m_μ
        it "s_(2,1) in monomial basis: s_(2,1) = 2*m_(1,1,1) + m_(2,1)" $ do
            let result = fromSchur (s (Partition [2,1])) :: SymFun Mono
            coeffOf (Partition [1,1,1]) result `shouldBe` 2
            coeffOf (Partition [2,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 2

        it "s_(3) = m_(3) + m_(2,1) + m_(1,1,1)" $ do
            let result = fromSchur (s (Partition [3])) :: SymFun Mono
            coeffOf (Partition [3]) result `shouldBe` 1
            coeffOf (Partition [2,1]) result `shouldBe` 1
            coeffOf (Partition [1,1,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 3

        it "s_(1,1,1) = m_(1,1,1)" $ do
            let result = fromSchur (s (Partition [1,1,1])) :: SymFun Mono
            coeffOf (Partition [1,1,1]) result `shouldBe` 1
            length (terms result) `shouldBe` 1

        -- Round-trip: m -> s -> m = id
        it "round-trip m -> s -> m is identity" $ do
            let original = m (Partition [2,1])
                roundTrip = fromSchur (toSchur original) :: SymFun Mono
            roundTrip `shouldBe` original

        -- Round-trip: s -> m -> s = id
        it "round-trip s -> m -> s is identity" $ do
            let original = s (Partition [2,1])
                asM = fromSchur original :: SymFun Mono
                roundTrip = toSchur asM
            roundTrip `shouldBe` original

    describe "mulM" $ do
        it "m_(1) * m_(1) via Schur gives correct result" $ do
            let result = mulM (m (Partition [1])) (m (Partition [1]))
            -- m_1 * m_1 = m_2 + 2*m_{1,1} (well-known)
            -- Actually: m_1 = s_1, so m_1^2 = s_1^2 = s_2 + s_{1,1} = m_2 + 2*m_{1,1}
            coeffOf (Partition [2]) result `shouldBe` 1
            coeffOf (Partition [1,1]) result `shouldBe` 2
