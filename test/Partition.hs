module Partition (Partition.test) where

import qualified Conway.Partition as Partition
import qualified Conway.World as World
import qualified Data.HashMap.Strict as Map
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "partition"
    [ testCase
        "partition/dividable-0"
        ( do
            let world = World.World {World.width = 3, World.height = 3, World.grid = Map.empty}
                slices = Partition.partition 1 1 world

            assertEqual "" 9 (length slices)
        ),
      testCase
        "partition/dividable-1"
        ( do
            let world = World.World {World.width = 3, World.height = 3, World.grid = Map.empty}
                slices = Partition.partition 3 1 world

            assertEqual "" 3 (length slices)
        ),
      testCase
        "partition/not-dividable-0"
        ( do
            let world = World.World {World.width = 3, World.height = 3, World.grid = Map.empty}
                slices = Partition.partition 2 1 world

            assertEqual "" 6 (length slices)
        ),
      testCase
        "partition/not-dividable-1"
        ( do
            let world = World.World {World.width = 3, World.height = 3, World.grid = Map.empty}
                slices = Partition.partition 2 2 world

            assertEqual "" 4 (length slices)
        ),
      testCase
        "partition/not-dividable-2"
        ( do
            let world = World.World {World.width = 7, World.height = 3, World.grid = Map.empty}
                slices = Partition.partition 2 1 world

            assertEqual "" 12 (length slices)
        )
    ]
