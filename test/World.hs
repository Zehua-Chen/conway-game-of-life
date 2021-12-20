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
        "fromList"
        ( do
            world <-
              World.fromList
                [[True, True, True], [True, False, True], [True, True, True]]

            assertEqual "width" 3 (World.width world)
            assertEqual "width" 3 (World.height world)

            assertEqual "live count" 8 (World.liveCount world)
        ),
      testCase
        "minX, maxX, minY, maxY"
        ( do
            assertEqual "" (-2) (World.minX 5)
            assertEqual "" 2 (World.maxX 5)

            assertEqual "" (-2) (World.minY 5)
            assertEqual "" 2 (World.maxY 5)
        )
    ]
