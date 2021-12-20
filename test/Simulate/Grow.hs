module Simulate.Grow (Simulate.Grow.test) where

import qualified Conway.Simulate as Conway
import qualified Conway.World as World
import qualified Data.HashMap.Strict as Map
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "simulate - grow"
    [ testCase
        "not grow"
        ( do
            world <-
              World.fromList
                [[False, False, False], [False, False, False], [False, False, False]]

            newWorld <- Conway.grow world

            assertEqual "size does not grow" 3 (World.width newWorld)
            assertEqual "size does not grow" 3 (World.height newWorld)
            assertEqual "" 9 (length $ World.grid newWorld)
        ),
      testCase
        "grow"
        ( do
            world <-
              World.fromList
                [[True, True, True], [False, False, False], [False, False, False]]

            newWorld <- Conway.grow world

            let grid = World.grid newWorld

            assertEqual "" (Just True) (Map.lookup (0, 2) grid)

            assertEqual "width does not grow" 3 (World.width newWorld)
            assertEqual "height does grow" 5 (World.height newWorld)
            assertEqual "" 15 (length grid)
        )
    ]
