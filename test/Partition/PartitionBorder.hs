module Partition.PartitionBorder (Partition.PartitionBorder.test) where

import qualified Conway.Partition as Partition
import qualified Conway.World as World
import qualified Data.HashSet as Set
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "partition/border"
    [ testCase
        "partition/border/dividable-0"
        ( do
            let world = World.fromWH 3 3
                borders = Partition.partitionBorders 1 1 world
                expected =
                  [ (-1, -1),
                    (0, -1),
                    (1, -1),
                    (-1, 0),
                    (0, 0),
                    (1, 0),
                    (-1, 1),
                    (0, 1),
                    (1, 1)
                  ]

            assertEqual "size" 9 (length borders)
            assertEqual "equality" (Set.fromList expected) borders
        ),
      testCase
        "partition/border/dividable-1"
        ( do
            let world = World.fromWH 9 9
                borders = Partition.partitionBorders 3 3 world

            assertEqual "size" 56 (length borders)
        ),
      testCase
        "partition/border/dividable-2"
        ( do
            let world = World.fromWH 3 3
                borders = Partition.partitionBorders 3 3 world

            assertEqual "size" 0 (length borders)
        ),
      testCase
        "partition/border/not-dividable-0"
        ( do
            let world = World.fromWH 5 5
                borders = Partition.partitionBorders 2 2 world
                expected =
                  [ (-2, 2),
                    (-1, 2),
                    (0, 2),
                    (1, 2),
                    (2, 2),
                    (-2, 1),
                    (-1, 1),
                    (0, 1),
                    (1, 1),
                    (2, 1),
                    (-2, 0),
                    (-1, 0),
                    (0, 0),
                    (1, 0),
                    (2, 0),
                    (-2, -1),
                    (-1, -1),
                    (0, -1),
                    (1, -1),
                    (2, -1),
                    (-1, -2),
                    (0, -2),
                    (1, -2),
                    (2, -2)
                  ]

            assertEqual "size" 24 (length borders)
            assertEqual "equality" (Set.fromList expected) borders
        )
    ]
