{-# LANGUAGE TupleSections #-}

module Conway.Simulate (simulate, grow, simulateSync, simulateAsync) where

import qualified Conway.Partition as Partition
import Conway.World
import Data.Foldable
import qualified Data.HashMap.Strict as Map

-- import Debug.Trace

simulateCells :: (Foldable m) => m Vec2 -> World -> [(Vec2, Bool)]
simulateCells cells oldWorld = foldr _simulate [] cells
  where
    _simulate :: Vec2 -> [(Vec2, Bool)] -> [(Vec2, Bool)]
    _simulate pos@(x, y) simulated =
      let live = liveNeighbors oldWorld x y
       in if getCell oldWorld pos
            then
              ( if live == 2 || live == 3
                  then (pos, True) : simulated
                  else (pos, False) : simulated
              )
            else
              ( if live == 3
                  then (pos, True) : simulated
                  else (pos, False) : simulated
              )

grow :: World -> IO World
grow world = do
  Conway.World.guard world

  -- four corners of the expanded grid does not need to be simulated,
  -- as they will never have 3 live neighbors in order to be alive

  let xs = [(minX world) .. (maxX world)]
      ys = [(minY world) .. (maxY world)]
      top = map (,maxY world + 1) xs
      bottom = map (,minY world - 1) xs
      left = map (minX world - 1,) ys
      right = map (maxX world + 1,) ys
      cellsTB = top ++ bottom
      cellsLR = left ++ right
      growCellsTB = simulateCells cellsTB world
      growCellsLR = simulateCells cellsLR world
      growSize :: (Vec2, Bool) -> Int -> Int
      growSize (_, live) count = if live then count + 1 else count
      growSizeTB = foldr growSize 0 growCellsTB
      growSizeLR = foldr growSize 0 growCellsLR
      newGrid =
        Map.union
          (grid world)
          ( Map.union
              (if growSizeTB > 0 then Map.fromList growCellsTB else Map.empty)
              (if growSizeLR > 0 then Map.fromList growCellsLR else Map.empty)
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
      let xs = [(minX broken) .. (maxX broken)]
          ys = [(minY broken) .. (maxY broken)]
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
      newWorld = setCells world (simulateCells xys world)

  return newWorld

simulateSync :: World -> IO World
simulateSync old = do
  new <- simulate (Partition.fromWorld old) old
  grown <- grow old

  return $ stack new grown

simulateAsync :: Int -> Int -> World -> IO World
simulateAsync sliceWidth sliceHeight old = do
  newWorld <- foldrM simulate old slices
  let partitionBorderCells = simulateCells (Partition.partitionBorders sliceWidth sliceHeight old) old
      newWorldWithBorder = setCells newWorld partitionBorderCells
  grown <- grow old
  return $ stack newWorldWithBorder grown
  where
    slices = Partition.partition sliceWidth sliceHeight old
