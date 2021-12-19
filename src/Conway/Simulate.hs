{-# LANGUAGE TupleSections #-}

module Conway.Simulate (simulate, grow) where

import Conway.World
import qualified Data.HashMap.Strict as Map

simulateCells :: [(Int, Int)] -> Grid -> Grid
simulateCells cells oldGrid = foldr (_simulate oldGrid) Map.empty cells
  where
    _simulate :: Grid -> (Int, Int) -> Grid -> Grid
    _simulate oldWorld pos@(x, y) newGrid =
      let live = liveNeighbors x y oldWorld
       in case Map.lookup pos oldWorld of
            Just alive ->
              if alive
                then
                  if live == 2 || live == 3
                    then Map.insert pos True newGrid
                    else Map.insert pos False newGrid
                else
                  if live == 3
                    then Map.insert pos True newGrid
                    else Map.insert pos False newGrid
            Nothing ->
              if live == 3
                then Map.insert pos True newGrid
                else Map.insert pos False newGrid

grow :: World -> IO World
grow world = do
  Conway.World.guard world

  let xs = [(minX $ width world - 1) .. (maxX $ width world + 1)]
      ys = [(minY $ height world) .. (maxY $ height world)]
      top = map (,maxY $ height world + 1) xs
      bottom = map (,minY $ height world - 1) xs
      left = map (minX $ width world - 1,) ys
      right = map (maxX $ width world + 1,) ys
      cellsTB = top ++ bottom
      cellsLR = left ++ right
      growGridTB = simulateCells cellsTB (grid world)
      growGridLR = simulateCells cellsLR (grid world)
      growSizeTB =
        foldr
          (\cell count -> if cell then count + 1 else count)
          (0 :: Int)
          growGridTB
      growSizeLR =
        foldr
          (\cell count -> if cell then count + 1 else count)
          (0 :: Int)
          growGridLR
      newGrid = Map.union (grid world) (Map.union growGridTB growGridLR)

  return
    World
      { width = if growSizeLR > 0 then width world + 1 else width world,
        height = if growSizeTB > 0 then height world + 1 else height world,
        grid = newGrid
      }

simulate :: World -> IO World
simulate world = do
  Conway.World.guard world

  let xs = [(minX $ width world) .. (maxX $ width world)]
      ys = [(minY $ height world) .. (maxY $ height world)]
      xys = concatMap (\x -> map (x,) ys) xs
      newGrid = simulateCells xys (grid world)

  return World {width = width world, height = height world, grid = newGrid}
