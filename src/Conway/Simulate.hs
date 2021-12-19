{-# LANGUAGE TupleSections #-}

module Conway.Simulate where

import Control.Monad (guard)
import Conway.World
import qualified Data.HashMap.Strict as Map

simulate :: World -> IO World
simulate world = do
  guard (odd $ width world)
  guard (odd $ height world)

  let xs = [(minX $ width world) .. (maxX $ width world)]
      ys = [(minY $ height world) .. (maxY $ height world)]
      xys = concatMap (\x -> map (x,) ys) xs
      newGrid = foldr (_simulate (grid world)) Map.empty xys
   in do
        return World {width = width world, height = height world, grid = newGrid}
  where
    _simulate :: Grid -> (Int, Int) -> Grid -> Grid
    _simulate oldWorld pos@(x, y) newGrid = case Map.lookup pos oldWorld of
      Just alive ->
        let live = liveNeighbors
         in if alive
              then
                if live == 2 || live == 3
                  then Map.insert pos True newGrid
                  else Map.insert pos False newGrid
              else
                if live == 3
                  then Map.insert pos True newGrid
                  else Map.insert pos False newGrid
      Nothing -> undefined
      where
        neighbors =
          [ (x + 1, y),
            (x - 1, y),
            (x, y + 1),
            (x, y - 1),
            (x + 1, y + 1),
            (x + 1, y - 1),
            (x - 1, y + 1),
            (x - 1, y - 1)
          ]

        liveNeighbors :: Int
        liveNeighbors =
          foldr
            ( \neighbor count ->
                case Map.lookup neighbor oldWorld of
                  Just alive -> if alive then count + 1 else count
                  Nothing -> count
            )
            0
            neighbors
