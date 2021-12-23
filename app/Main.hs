module Main where

import qualified Benchmark
import qualified Conway.Json as Json
import qualified Conway.PPM as PPM
import qualified Conway.Simulate as Simulate
import System.Environment

program :: [String] -> IO ()
program ["benchmark"] = do
  Benchmark.benchmark
program [input, iterations, sliceWidth, sliceHeight, chunkSize] = do
  start <- Json.worldFromJson input
  -- https://stackoverflow.com/questions/20667478/haskell-string-int-type-conversion
  let iterations' = read iterations :: Int
      sliceWidth' = read sliceWidth :: Int
      sliceHeight' = read sliceHeight :: Int
      chunkSize' = read chunkSize :: Int
      sync = foldr (\_ w -> Simulate.simulateSync w) start [0 .. iterations']
      async = foldr (\_ w -> Simulate.simulateAsync sliceWidth' sliceHeight' chunkSize' w) start [0 .. iterations']

  PPM.save sync "sync.ppm"
  PPM.save async "async.ppm"
program args = do
  putStr $
    "args not recognized: " ++ show args ++ "\n"
      ++ "zc2616's final project\n"
      ++ "  benchmark: run bench mark\n"
      ++ "  <file> <iterations> <slice width> <slice height> <chunk size>: simulate file\n"

main :: IO ()
main = do
  args <- getArgs
  program args
