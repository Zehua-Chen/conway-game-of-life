module Main where

import qualified Benchmark
import qualified Conway.Simulate as Simulate
import qualified Conway.World as World
import System.Environment

program :: [String] -> IO ()
program ["benchmark"] = do
  Benchmark.benchmark
program [input] = do
  print ("simulate file " ++ input)
program _ = do
  putStr $
    "zc2616's final project\n"
      ++ "  benchmark: run bench mark\n"
      ++ "  <file>: simulate file\n"

main :: IO ()
main = do
  args <- getArgs
  program args
