module QHask.Sym.TypesSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map

import QHask.Partition (Partition(..))
import QHask.Sym.Types

spec :: Spec
spec = do
    describe "zero" $ do
        it "has no terms" $
            terms (zero :: SymFun Schur) `shouldBe` []

    describe "add" $ do
        it "adds coefficients" $ do
            let a = SymFun (Map.singleton (Partition [2,1]) 1) :: SymFun Schur
                b = SymFun (Map.singleton (Partition [2,1]) 2) :: SymFun Schur
            coeffOf (Partition [2,1]) (add a b) `shouldBe` 3
        it "cancels to zero" $ do
            let a = SymFun (Map.singleton (Partition [2,1]) 3) :: SymFun Schur
                b = SymFun (Map.singleton (Partition [2,1]) (-3)) :: SymFun Schur
            add a b `shouldBe` zero

    describe "scale" $ do
        it "multiplies coefficients" $ do
            let f = SymFun (Map.singleton (Partition [3]) 2) :: SymFun Schur
            coeffOf (Partition [3]) (scale 5 f) `shouldBe` 10
        it "scale by 0 gives zero" $ do
            let f = SymFun (Map.singleton (Partition [3]) 2) :: SymFun Schur
            scale 0 f `shouldBe` zero

    describe "degree" $ do
        it "returns max weight" $ do
            let f = SymFun (Map.fromList [(Partition [3], 1), (Partition [2,1], 1)]) :: SymFun Schur
            degree f `shouldBe` 3
        it "zero has degree 0" $
            degree (zero :: SymFun Schur) `shouldBe` 0

    describe "isHomogeneous" $ do
        it "single basis element is homogeneous" $ do
            let f = SymFun (Map.singleton (Partition [2,1]) 1) :: SymFun Schur
            isHomogeneous f `shouldBe` True
        it "mixed degrees is not homogeneous" $ do
            let f = SymFun (Map.fromList [(Partition [3], 1), (Partition [2], 1)]) :: SymFun Schur
            isHomogeneous f `shouldBe` False

    describe "showSym" $ do
        it "prints zero" $
            showSym "s" (zero :: SymFun Schur) `shouldBe` "0"
        it "prints a single term" $ do
            let f = SymFun (Map.singleton (Partition [2,1]) 1) :: SymFun Schur
            showSym "s" f `shouldBe` "s_(2,1)"
