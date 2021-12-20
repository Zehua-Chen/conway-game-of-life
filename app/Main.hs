module Main where

import Conway.Simulate
import Conway.World as World

main :: IO ()
main = do
  world <-
    World.fromList
      [ [True, True, True, True, True],
        [False, True, False, False, False],
        [False, True, False, True, False],
        [True, True, False, True, True],
        [True, True, True, True, True]
      ]

  print world

  let syncWorld = foldr (\_ w -> simulateSync w) world iterations
  print "sync done"
  print syncWorld

  let asyncWorld = foldr (\_ w -> simulateAsync 4 4 w) world iterations

  print "async done"
  print asyncWorld
  where
    -- print syncWorld
    -- print asyncWorld

    iterations = [0 :: Int .. 10000]
