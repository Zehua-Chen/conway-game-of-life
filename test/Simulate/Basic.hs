module Simulate.Basic (Simulate.Basic.test) where

import qualified Conway.Simulate as Conway
import qualified Conway.World as World
import qualified Data.HashMap.Strict as Map
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "simulate - basic"
    [ testCase
        "simulate 1"
        ( do
            world <-
              World.fromList
                [[True, False, False], [False, True, True], [False, False, False]]

            newWorld <- Conway.simulate world

            let newGrid = World.grid newWorld

            assertEqual "two live cells" 2 (World.liveCount newWorld)
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
