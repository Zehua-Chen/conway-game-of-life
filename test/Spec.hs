import qualified Conway.Simulate as Conway
import qualified Conway.World as World
import qualified Data.HashMap.Strict as Map
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

liveCount :: World.World -> Int
liveCount world = foldr (\cell count -> if cell then count + 1 else count) (0 :: Int) (World.grid world)

tests :: [Test.Framework.Test]
tests =
  [ testGroup
      "World"
      [ testCase
          "fromList"
          ( do
              world <-
                World.fromList
                  [[True, True, True], [True, False, True], [True, True, True]]

              assertEqual "width" 3 (World.width world)
              assertEqual "width" 3 (World.height world)

              assertEqual "live count" 8 (liveCount world)
          )
      ],
    testGroup
      "Simulate"
      [ testCase
          "simulate 1"
          ( do
              world <-
                World.fromList
                  [[True, False, False], [False, True, True], [False, False, False]]

              newWorld <- Conway.simulate world

              let newGrid = World.grid newWorld

              assertEqual "two live cells" 2 (liveCount newWorld)
              assertEqual "two live cells" (Just True) (Map.lookup (0, 0) newGrid)
              assertEqual "two live cells" (Just True) (Map.lookup (0, -1) newGrid)
          ),
        testCase
          "simulate 2"
          ( do
              world <-
                World.fromList
                  [[True, True, True], [True, True, True], [True, True, True]]

              newWorld <- Conway.simulate world

              let newGrid = World.grid newWorld

              assertEqual "center cell dies" (Just False) (Map.lookup (0, 0) newGrid)
          )
      ]
  ]

main :: IO ()
main = defaultMain tests
