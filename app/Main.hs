module Main where

import Conway.Simulate
import Conway.World as World
import Criterion.Main

bigWorld :: World
bigWorld = World.fromList (replicate 101 (replicate 101 True))

main :: IO ()
main =
  defaultMain
    [ bench "async" async,
      bench "sync" sync
    ]
  where
    async = nf (simulateAsync 40 40) bigWorld
    sync = nf simulateSync bigWorld
