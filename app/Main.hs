module Main where

import Conway.Simulate
import Conway.World as World

main :: IO ()
main = do
  world <-
    World.fromList
      [ [True, True, False, True, True],
        [False, True, False, False, False],
        [False, True, False, True, False],
        [True, True, False, True, True],
        [False, True, False, False, False]
      ]

  print world

  syncWorld <- simulateSync world
  asyncWorld <- simulateAsync 3 3 world

  print syncWorld
  print asyncWorld
