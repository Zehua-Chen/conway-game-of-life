module Main where

import Conway.PPM as PPM
import Conway.Simulate
import Conway.World as World
import Criterion.Main

run :: Int
run = undefined

bigWorld :: World
bigWorld = World.fromList (replicate 101 (replicate 101 True))

a = nf (+ 1) (1 :: Int)

main :: IO ()
main =
  defaultMain
    [ bench "" a
    ]
  where
    iterations = [0 :: Int .. 1]
