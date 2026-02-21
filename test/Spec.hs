import Test.Hspec

import qualified QHask.QSeriesSpec
import qualified QHask.PartitionSpec
import qualified QHask.ProfileSpec
import qualified QHask.CPPSpec
import qualified QHask.ConjectureSpec

main :: IO ()
main = hspec $ do
    describe "QHask.QSeries" QHask.QSeriesSpec.spec
    describe "QHask.Partition" QHask.PartitionSpec.spec
    describe "QHask.Profile" QHask.ProfileSpec.spec
    describe "QHask.CPP" QHask.CPPSpec.spec
    describe "QHask.Conjecture" QHask.ConjectureSpec.spec
