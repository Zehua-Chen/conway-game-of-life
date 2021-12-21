module Main where

import Conway.PPM as PPM
import Conway.Simulate
import Conway.World as World

main :: IO ()
main = do
  world <-
    World.fromList (replicate 101 (replicate 101 True))

  print world

  -- let syncWorld = foldr (\_ w -> simulateSync w) world iterations
  -- print "sync done"
  -- print syncWorld

  let asyncWorld = foldr (\_ w -> simulateAsync 21 21 w) world iterations

  print "async done"
  PPM.save asyncWorld "async.ppm"
  where
    -- print syncWorld
    -- print asyncWorld

    iterations = [0 :: Int .. 1]
