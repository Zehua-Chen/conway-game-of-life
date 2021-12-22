module Main where

import Conway.Simulate
import Conway.World as World
import Criterion.Main

bigWorld :: World
bigWorld = World.fromList (replicate 101 (replicate 101 True))

main :: IO ()
main =
  defaultMain
    [ bench "sync" sync,
      bgroup
        "async"
        [ bench "w: 10, h: 10, chunk: 10" (async 10 10 10),
          bench "w: 20, h: 20, chunk: 10" (async 20 20 10),
          bench "w: 40, h: 40, chunk: 10" (async 40 40 10),
          bench "w: 80, h: 80, chunk: 10" (async 80 80 10),
          bench "w: 40, h: 40, chunk: 10" (async 40 40 10),
          bench "w: 40, h: 40, chunk: 20" (async 40 40 20),
          bench "w: 40, h: 40, chunk: 40" (async 40 40 40),
          bench "w: 40, h: 40, chunk: 80" (async 40 40 80)
        ]
    ]
  where
    sync = nf simulateSync bigWorld
    async w h c = nf (simulateAsync w h c) bigWorld
