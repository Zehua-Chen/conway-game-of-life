{-# LANGUAGE TupleSections #-}

module Conway.Simulate (simulate, grow, simulateSync, simulateAsync) where

import Control.Parallel.Strategies
import qualified Conway.Partition as Partition
import Conway.World
import Data.Foldable

-- import Debug.Trace

simulateCell :: World -> Vec2 -> (Vec2, Bool)
simulateCell world pos@(x, y) =
  let live = liveNeighbors world x y
   in if getCell world pos
        then
          ( if live == 2 || live == 3
              then (pos, True)
              else (pos, False)
          )
        else
          ( if live == 3
              then (pos, True)
              else (pos, False)
          )

simulateCells :: [Vec2] -> World -> [(Vec2, Bool)]
simulateCells cells oldWorld = map (simulateCell oldWorld) cells

grow :: World -> Eval World
grow world = do
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
      grownWorld =
        fromWH
          (if growSizeLR > 0 then width world + 2 else width world)
          (if growSizeTB > 0 then height world + 2 else height world)

  return
    ( setCells
        (setCells grownWorld (if growSizeLR > 0 then growCellsLR else []))
        (if growSizeTB > 0 then growCellsTB else [])
    )

simulate :: Partition.Slice -> World -> Eval World
simulate slice world = do
  let xs = [(Partition.minX slice) .. (Partition.maxX slice)]
      ys = [(Partition.minY slice) .. (Partition.maxY slice)]
      xys = concatMap (\x -> map (x,) ys) xs
      newWorld = setCells world (simulateCells xys world)

  return newWorld

simulateSync :: World -> IO World
simulateSync old = return $
  runEval $ do
    new <- simulate (Partition.fromWorld old) old
    grown <- grow old

    return $ stack new grown

simulateAsync :: Int -> Int -> World -> IO World
simulateAsync sliceWidth sliceHeight old = return $
  runEval $ do
    newWorld <- foldrM simulate old slices
    let partitionBorderCells = simulateCells (toList $ Partition.partitionBorders sliceWidth sliceHeight old) old
        newWorldWithBorder = setCells newWorld partitionBorderCells
    grown <- grow old
    return $ stack newWorldWithBorder grown
  where
    slices = Partition.partition sliceWidth sliceHeight old
