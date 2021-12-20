module World (World.test) where

import qualified Conway.World as World
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

test :: Test.Framework.Test
test =
  testGroup
    "world"
    [ testCase
        "world/fromList"
        ( do
            world <-
              World.fromList
                [[True, True, True], [True, False, True], [True, True, True]]

            assertEqual "width" 3 (World.width world)
            assertEqual "width" 3 (World.height world)

            assertEqual "live count" 8 (World.liveCount world)
        ),
      testCase
        "world/minX,maxX,minY,maxY"
        ( do
            let world = World.fromWH 5 5
            assertEqual "" (-2) (World.minX world)
            assertEqual "" 2 (World.maxX world)

            assertEqual "" (-2) (World.minY world)
            assertEqual "" 2 (World.maxY world)
        ),
      testCase
        "world/setCell,getCell"
        ( do
            let world = World.fromWH 1 1

            assertEqual "" False (World.getCell world (0, 0))

            let newWorld = World.setCell world (0, 0) True
            assertEqual "" True (World.getCell newWorld (0, 0))
        ),
      testCase
        "world/stack"
        ( do
            a <- World.fromList [[False]]
            b <- World.fromList [[True, True, True]]

            let result = a `World.stack` b

            assertEqual "width" 3 (World.width result)
            assertEqual "height" 1 (World.height result)

            assertEqual "(-1, 0)" True (World.getCell result (-1, 0))
            assertEqual "(0, 0)" False (World.getCell result (0, 0))
            assertEqual "(1, 0)" True (World.getCell result (1, 0))
        )
    ]
