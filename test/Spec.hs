import Test.Hspec

import qualified QHask.QSeriesSpec
import qualified QHask.PartitionSpec
import qualified QHask.ProfileSpec
import qualified QHask.CPPSpec
import qualified QHask.ConjectureSpec
import qualified QHask.Sym.TypesSpec
import qualified QHask.Sym.KostkaSpec
import qualified QHask.Sym.LRSpec
import qualified QHask.Sym.SchurSpec
import qualified QHask.Sym.MonomialSpec
import qualified QHask.Sym.HomogeneousSpec
import qualified QHask.Sym.ElementarySpec
import qualified QHask.Sym.PowerSumSpec
import qualified QHask.Sym.SymSpec

main :: IO ()
main = hspec $ do
    describe "QHask.QSeries" QHask.QSeriesSpec.spec
    describe "QHask.Partition" QHask.PartitionSpec.spec
    describe "QHask.Profile" QHask.ProfileSpec.spec
    describe "QHask.CPP" QHask.CPPSpec.spec
    describe "QHask.Conjecture" QHask.ConjectureSpec.spec
    describe "QHask.Sym.Types" QHask.Sym.TypesSpec.spec
    describe "QHask.Sym.Kostka" QHask.Sym.KostkaSpec.spec
    describe "QHask.Sym.LR" QHask.Sym.LRSpec.spec
    describe "QHask.Sym.Schur" QHask.Sym.SchurSpec.spec
    describe "QHask.Sym.Monomial" QHask.Sym.MonomialSpec.spec
    describe "QHask.Sym.Homogeneous" QHask.Sym.HomogeneousSpec.spec
    describe "QHask.Sym.Elementary" QHask.Sym.ElementarySpec.spec
    describe "QHask.Sym.PowerSum" QHask.Sym.PowerSumSpec.spec
    describe "QHask.Sym" QHask.Sym.SymSpec.spec
