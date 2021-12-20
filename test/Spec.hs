import qualified Partition.Partition
import qualified Partition.PartitionBorder
import qualified Simulate.Async
import qualified Simulate.Basic
import qualified Simulate.Grow
import Test.Framework (Test, defaultMain)
import qualified World

tests :: [Test.Framework.Test]
tests =
  [ World.test,
    Partition.Partition.test,
    Partition.PartitionBorder.test,
    Simulate.Basic.test,
    Simulate.Grow.test,
    Simulate.Async.test
  ]

main :: IO ()
main = defaultMain tests
