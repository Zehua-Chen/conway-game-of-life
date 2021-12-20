module Simulate.Async (Simulate.Async.test) where

import qualified Conway.Simulate as Conway
import qualified Conway.World as World
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "simulate/async"
    [ testCase
        "simulate/async/3x3"
        ( do
            world <-
              World.fromList
                [ [True, True, False],
                  [False, True, False],
                  [False, True, False]
                ]

            syncWorld <- Conway.simulateSync world
            asyncWorld <- Conway.simulateAsync 1 1 world

            assertEqual "" syncWorld asyncWorld
        ),
      testCase
        "simulate/async/5x5"
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

            syncWorld <- Conway.simulateSync world
            asyncWorld <- Conway.simulateAsync 3 3 world

            -- print syncWorld

            assertEqual "width" (World.width syncWorld) (World.width asyncWorld)
            assertEqual "height" (World.height syncWorld) (World.height asyncWorld)
            assertEqual "" syncWorld asyncWorld
        ),
      testCase
        "simulate/async/9x9"
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

            syncWorld <- Conway.simulateSync world
            asyncWorld <- Conway.simulateAsync 3 3 world

            assertEqual "" syncWorld asyncWorld
        )
    ]
