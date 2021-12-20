module Simulate.Finite (Simulate.Finite.test) where

import qualified Conway.Partition as Partition
import qualified Conway.Simulate as Conway
import qualified Conway.World as World
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "simulate/finite"
    [ testCase
        "simulate/finite/0"
        ( do
            world <-
              World.fromList
                [[True, False, False], [False, True, True], [False, False, False]]

            newWorld <- Conway.simulate (Partition.fromWorld world) world

            let newGrid = World.grid newWorld

            assertEqual "exactly 9 cells" 9 (length newGrid)
            assertEqual "two live cells" 2 (World.liveCount newWorld)
            assertEqual "two live cells" True (World.getCell newWorld (0, 0))
            assertEqual "two live cells" True (World.getCell newWorld (0, 1))
        ),
      testCase
        "simulate/finite/1"
        ( do
            world <-
              World.fromList
                [[True, True, True], [True, True, True], [True, True, True]]

            newWorld <- Conway.simulate (Partition.fromWorld world) world

            assertEqual "" 9 (length $ World.grid world)
            assertEqual "center cell dies" False (World.getCell newWorld (0, 0))
        )
    ]
