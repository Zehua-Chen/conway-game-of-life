module Simulate.Infinite (Simulate.Infinite.test) where

import qualified Conway.Simulate as Conway
import qualified Conway.World as World
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "simulate/infinite"
    [ testCase
        "simulate/infinite/3x3-not-growing"
        ( do
            world <-
              World.fromList
                [ [True, True, False],
                  [False, True, False],
                  [False, True, False]
                ]

            let syncWorld = Conway.simulateSync world
            let asyncWorld = Conway.simulateAsync 1 1 world

            assertEqual "width" 3 (World.width syncWorld)
            assertEqual "height" 3 (World.height syncWorld)
            assertEqual "# of live cells" 4 (World.liveCount syncWorld)
            assertEqual "# of cells" 9 (length $ World.grid syncWorld)

            assertEqual "" syncWorld asyncWorld
        ),
      testCase
        "simulate/infinite/3x3-growing"
        ( do
            world <-
              World.fromList
                [ [True, True, True],
                  [False, True, False],
                  [False, True, False]
                ]

            let syncWorld = Conway.simulateSync world
            let asyncWorld = Conway.simulateAsync 1 1 world

            assertEqual "width" 3 (World.width syncWorld)
            assertEqual "height" 5 (World.height syncWorld)
            assertEqual "# of live cells" 4 (World.liveCount syncWorld)
            assertEqual "# of cells" 15 (length $ World.grid syncWorld)

            assertEqual "" syncWorld asyncWorld
        ),
      testCase
        "simulate/infinite/5x5"
        ( do
            world <-
              World.fromList
                [ [True, True, False, True, True],
                  [False, True, False, False, False],
                  [False, True, False, True, False],
                  [True, True, False, True, True],
                  [False, True, False, False, False]
                ]

            -- print world

            let syncWorld = Conway.simulateSync world
            let asyncWorld = Conway.simulateAsync 3 3 world

            -- print syncWorld
            -- print (World.grid asyncWorld)

            assertEqual "# of live cells" 16 (World.liveCount syncWorld)
            assertEqual "width" 5 (World.width syncWorld)
            assertEqual "width" 5 (World.height syncWorld)

            assertEqual "width" (World.width syncWorld) (World.width asyncWorld)
            assertEqual "height" (World.height syncWorld) (World.height asyncWorld)

            assertEqual "same number of living cells" (World.liveCount syncWorld) (World.liveCount asyncWorld)
            assertEqual "same number of cells" (length $ World.grid syncWorld) (length $ World.grid asyncWorld)
            assertEqual "equality" syncWorld asyncWorld
        ),
      testCase
        "simulate/infinite/9x9"
        ( do
            world <-
              World.fromList
                [ [True, True, False, True, False, True, False, True, True],
                  [True, True, False, True, False, True, False, True, True],
                  [True, True, False, True, False, True, False, True, True],
                  [True, True, False, True, False, True, False, True, True],
                  [True, True, False, True, False, True, False, True, True],
                  [True, True, False, True, False, True, False, True, True],
                  [True, True, False, True, False, True, False, True, True],
                  [True, True, False, True, False, True, False, True, True],
                  [True, True, False, True, False, True, False, True, True]
                ]

            let syncWorld = Conway.simulateSync world
            let asyncWorld = Conway.simulateAsync 3 3 world

            assertEqual "" (length $ World.grid syncWorld) (length $ World.grid asyncWorld)
            assertEqual "" syncWorld asyncWorld
        )
    ]
