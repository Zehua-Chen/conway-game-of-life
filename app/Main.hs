module Main where

import qualified Conway.Simulate as Simulate
import qualified Conway.World as World
import Criterion.Main

bigWorld :: World.World
bigWorld = World.fromList (replicate 101 (replicate 101 True))

mediumWorld :: World.World
mediumWorld = World.fromList (replicate 51 (replicate 51 True))

simulate :: Int -> (World.World -> World.World) -> (World.World -> World.World)
simulate count f start = foldr (\_ w -> f w) start [1 .. count]

benchSingleIteration :: Benchmark
benchSingleIteration =
  bgroup
    "single iteration, world of size 101 x 101"
    [ bench "sync" sync,
      bench "w: 10, h: 10, chunk: 10" (async 10 10 10),
      bench "w: 20, h: 20, chunk: 10" (async 20 20 10),
      bench "w: 40, h: 40, chunk: 10" (async 40 40 10),
      bench "w: 80, h: 80, chunk: 10" (async 80 80 10),
      bench "w: 40, h: 40, chunk: 10" (async 40 40 10),
      bench "w: 40, h: 40, chunk: 20" (async 40 40 20),
      bench "w: 40, h: 40, chunk: 40" (async 40 40 40),
      bench "w: 40, h: 40, chunk: 80" (async 40 40 80)
    ]
  where
    sync = nf (simulate 1 Simulate.simulateSync) bigWorld
    async w h c = nf (simulate 1 (Simulate.simulateAsync w h c)) bigWorld

benchMultipleIterations :: Benchmark
benchMultipleIterations =
  bgroup
    "100 iterations, world of size 51 x 51"
    [ bench "sync" sync,
      bench "w: 10, h: 10, chunk: 10" (async 10 10 10),
      bench "w: 20, h: 20, chunk: 10" (async 20 20 10),
      bench "w: 40, h: 40, chunk: 10" (async 40 40 10),
      bench "w: 20, h: 20, chunk: 10" (async 20 20 10),
      bench "w: 20, h: 20, chunk: 20" (async 20 20 20),
      bench "w: 20, h: 20, chunk: 40" (async 20 20 40)
    ]
  where
    sync = nf (simulate 100 Simulate.simulateSync) mediumWorld
    async w h c = nf (simulate 100 (Simulate.simulateAsync w h c)) mediumWorld

main :: IO ()
main =
  defaultMain
    [ benchSingleIteration,
      benchMultipleIterations
    ]
