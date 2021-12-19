import qualified Simulate.Basic
import Test.Framework (Test, defaultMain)
import qualified World

tests :: [Test.Framework.Test]
tests =
  [ World.test,
    Simulate.Basic.test
  ]

main :: IO ()
main = defaultMain tests
