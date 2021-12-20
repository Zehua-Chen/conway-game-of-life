module Simulate.Grow (Simulate.Grow.test) where

import Control.Parallel.Strategies
import qualified Conway.Simulate as Conway
import qualified Conway.World as World
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "simulate/grow"
    [ testCase
        "simulate/grow/not-grow"
        ( do
            world <-
              World.fromList
                [[False, False, False], [False, False, False], [False, False, False]]

            let newWorld = runEval $ Conway.grow world

            assertEqual "size does not grow" 3 (World.width newWorld)
            assertEqual "size does not grow" 3 (World.height newWorld)
            assertEqual "exactly 9 cells" 9 (length $ World.grid newWorld)
        ),
      testCase
        "simulate/grow/grow-height"
        ( do
            world <-
              World.fromList
                [[True, True, True], [False, False, False], [False, False, False]]

            let newWorld = runEval $ Conway.grow world

            let grid = World.grid newWorld

            assertEqual "" True (World.getCell newWorld (0, 2))
            assertEqual "" False (World.getCell newWorld (-1, 2))
            assertEqual "" False (World.getCell newWorld (1, 2))

            assertEqual "width does not grow" 3 (World.width newWorld)
            assertEqual "height does grow" 5 (World.height newWorld)
            assertEqual "eactly 15 cells" 15 (length grid)
        ),
      testCase
        "simulate/grow/grow-width"
        ( do
            world <-
              World.fromList
                [[True, False, False], [True, False, False], [True, False, False]]

            let newWorld = runEval $ Conway.grow world

            let grid = World.grid newWorld

            assertEqual "" True (World.getCell newWorld (-2, 0))

            assertEqual "width does not grow" 5 (World.width newWorld)
            assertEqual "height does grow" 3 (World.height newWorld)
            assertEqual "" 15 (length grid)
        )
    ]
