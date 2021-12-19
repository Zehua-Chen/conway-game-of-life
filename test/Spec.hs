import qualified Partition
import qualified Simulate.Basic
import qualified Simulate.Grow
import Test.Framework (Test, defaultMain)
import qualified World

tests :: [Test.Framework.Test]
tests =
  [ World.test,
    Partition.test,
    Simulate.Basic.test,
    Simulate.Grow.test
  ]

main :: IO ()
main = defaultMain tests
