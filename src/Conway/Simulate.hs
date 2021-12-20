{-# LANGUAGE TupleSections #-}

module Conway.Simulate (simulate, grow, simulateSync, simulateAsync) where

import qualified Conway.Partition as Partition
import Conway.World
import Data.Foldable
import qualified Data.HashMap.Strict as Map

-- import Debug.Trace

simulateCells :: (Foldable m) => m (Int, Int) -> Grid -> Grid
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

  -- four corners of the expanded grid does not need to be simulated,
  -- as they will never have 3 live neighbors in order to be alive

  let xs = [(minX $ width world) .. (maxX $ width world)]
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
      newGrid =
        Map.union
          (grid world)
          ( Map.union
              (if growSizeTB > 0 then growGridTB else Map.empty)
              (if growSizeLR > 0 then growGridLR else Map.empty)
          )

  return $
    fillMissingCells
      World
        { width = if growSizeLR > 0 then width world + 2 else width world,
          height = if growSizeTB > 0 then height world + 2 else height world,
          grid = newGrid
        }
  where
    fillMissingCells :: World -> World
    fillMissingCells broken =
      let xs = [(minX $ width broken) .. (maxX $ width broken)]
          ys = [(minY $ height broken) .. (maxY $ height broken)]
          xys = concatMap (\x -> map (x,) ys) xs
          newGrid =
            foldr
              ( \xy w -> case Map.lookup xy w of
                  Just _ -> w
                  Nothing -> Map.insert xy False w
              )
              (grid broken)
              xys
       in World {width = width broken, height = height broken, grid = newGrid}

simulate :: Partition.Slice -> World -> IO World
simulate slice world = do
  Conway.World.guard world

  let xs = [(Partition.minX slice) .. (Partition.maxX slice)]
      ys = [(Partition.minY slice) .. (Partition.maxY slice)]
      xys = concatMap (\x -> map (x,) ys) xs
      newGrid = simulateCells xys (grid world)

  return World {width = width world, height = height world, grid = newGrid}

simulateSync :: World -> IO World
simulateSync old = do
  new <- simulate (Partition.fromWorld old) old
  grown <- grow old

  return World {width = width grown, height = height grown, grid = Map.union (grid grown) (grid new)}

simulateAsync :: Int -> Int -> World -> IO World
simulateAsync sliceWidth sliceHeight old = do
  newWorld <- foldrM simulate old slices
  let borderGrid = simulateCells (Partition.partitionBorders sliceWidth sliceHeight old) (grid old)
      newGrid = Map.unionWith const borderGrid (grid newWorld)
      newWorldWithBorder = World {width = width newWorld, height = height newWorld, grid = newGrid}
  grown <- grow old
  return World {width = width grown, height = height grown, grid = Map.union (grid grown) (grid newWorldWithBorder)}
  where
    slices = Partition.partition sliceWidth sliceHeight old
