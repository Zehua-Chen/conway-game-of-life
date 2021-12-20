import qualified Partition.Partition
import qualified Partition.PartitionBorder
import qualified Simulate.Finite
import qualified Simulate.Grow
import qualified Simulate.Infinite
import Test.Framework (Test, defaultMain)
import qualified World

tests :: [Test.Framework.Test]
tests =
  [ World.test,
    Partition.Partition.test,
    Partition.PartitionBorder.test,
    Simulate.Grow.test,
    Simulate.Finite.test,
    Simulate.Infinite.test
  ]

main :: IO ()
main = defaultMain tests
