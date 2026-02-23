module QHask.Sym.SymSpec (spec) where

import Test.Hspec

import QHask.Partition (Partition(..))
import QHask.Sym

spec :: Spec
spec = do
    describe "omega" $ do
        it "omega(s_λ) = s_{λ'}" $
            omega (s (Partition [2,1])) `shouldBe` s (Partition [2,1])
        it "omega(s_(3)) = s_(1,1,1)" $
            omega (s (Partition [3])) `shouldBe` s (Partition [1,1,1])
        it "omega(s_(2)) = s_(1,1)" $
            omega (s (Partition [2])) `shouldBe` s (Partition [1,1])
        it "omega is an involution" $ do
            let f = add (s (Partition [3])) (s (Partition [2,1]))
            omega (omega f) `shouldBe` f

    describe "innerProduct" $ do
        it "⟨s_λ, s_λ⟩ = 1" $
            innerProduct (s (Partition [2,1])) (s (Partition [2,1])) `shouldBe` 1
        it "⟨s_λ, s_μ⟩ = 0 for λ ≠ μ" $
            innerProduct (s (Partition [2,1])) (s (Partition [3])) `shouldBe` 0
        it "⟨s_(2)+s_(1,1), s_(2)⟩ = 1" $ do
            let f = add (s (Partition [2])) (s (Partition [1,1]))
            innerProduct f (s (Partition [2])) `shouldBe` 1

    describe "changeBasis" $ do
        -- h -> m round-trip through Schur
        it "h_(2,1) to monomial and back" $ do
            let hf = h (Partition [2,1])
                asM = changeBasis hf :: SymFun Mono
                back = changeBasis asM :: SymFun Homo
            back `shouldBe` hf

        -- e -> p round-trip
        it "e_(2,1) to power sum and back" $ do
            let ef = e (Partition [2,1])
                asP = changeBasis ef :: SymFun Pow
                back = changeBasis asP :: SymFun Elem
            back `shouldBe` ef

        -- omega(h_n) = e_n : verify via Schur basis
        it "omega(h_n) = e_n for n=3" $ do
            let h3_schur = toSchur (h (Partition [3]))
                e3_schur = toSchur (e (Partition [3]))
            omega h3_schur `shouldBe` e3_schur

    describe "mul" $ do
        it "generic mul on Homo: h_(2) * h_(1) = h_(2,1)" $
            mul (h (Partition [2])) (h (Partition [1]))
                `shouldBe` h (Partition [2,1])
