module Partition.Partition (Partition.Partition.test) where

import qualified Conway.Partition as Partition
import qualified Conway.World as World
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

assertValidSlices :: [Partition.Slice] -> World.World -> Assertion
assertValidSlices slices world =
  mapM_
    ( \slice -> do
        assertEqual "" True (Partition.minX slice >= World.minX world)
        assertEqual "" True (Partition.minY slice >= World.minY world)
        assertEqual "" True (Partition.maxX slice <= World.maxX world)
        assertEqual "" True (Partition.maxY slice <= World.maxY world)
    )
    slices

test :: Test.Framework.Test
test =
  testGroup
    "partition"
    [ testCase
        "partition/dividable-0"
        ( do
            let world = World.fromWH 3 3
                slices = Partition.partition 1 1 world

            assertEqual "" 9 (length slices)
            assertValidSlices slices world
        ),
      testCase
        "partition/dividable-1"
        ( do
            let world = World.fromWH 3 3
                slices = Partition.partition 3 1 world

            assertEqual "" 3 (length slices)
            assertValidSlices slices world
        ),
      testCase
        "partition/dividable-2"
        ( do
            let world = World.fromWH 9 9
                slices = Partition.partition 3 3 world

            assertEqual "" 9 (length slices)
            assertValidSlices slices world
        ),
      testCase
        "partition/not-dividable-0"
        ( do
            let world = World.fromWH 3 3
                slices = Partition.partition 2 1 world

            assertEqual "" 6 (length slices)
            assertValidSlices slices world
        ),
      testCase
        "partition/not-dividable-1"
        ( do
            let world = World.fromWH 3 3
                slices = Partition.partition 2 2 world

            assertEqual "" 4 (length slices)
            assertValidSlices slices world
        ),
      testCase
        "partition/not-dividable-2"
        ( do
            let world = World.fromWH 7 3
                slices = Partition.partition 2 1 world

            assertEqual "" 12 (length slices)
            assertValidSlices slices world
        )
    ]
