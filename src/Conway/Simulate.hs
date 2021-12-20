{-# LANGUAGE TupleSections #-}

module Conway.Simulate (simulate, grow, simulateSync, simulateAsync) where

import Control.Parallel.Strategies
import qualified Conway.Partition as Partition
import qualified Conway.Slice as Slice
import Conway.World
import Data.Foldable

-- import Debug.Trace

type SimulateResult = (Vec2, Bool)

simulateCell :: Slice.Slice -> World -> Vec2 -> SimulateResult
simulateCell slice world pos@(x, y) =
  let live = liveNeighbors slice world x y
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

simulateCells :: [Vec2] -> Slice.Slice -> World -> [SimulateResult]
simulateCells cells slice oldWorld = map (simulateCell slice oldWorld) cells

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
      growSlice = Slice.Slice {Slice.minX = minX world - 1, Slice.maxX = maxX world + 1, Slice.minY = minY world - 1, Slice.maxY = maxY world + 1}
      growCellsTB = simulateCells cellsTB growSlice world
      growCellsLR = simulateCells cellsLR growSlice world
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

simulate :: Slice.Slice -> World -> [SimulateResult]
simulate slice world = do
  let xs = [(Slice.minX slice) .. (Slice.maxX slice)]
      ys = [(Slice.minY slice) .. (Slice.maxY slice)]
      xys = concatMap (\x -> map (x,) ys) xs
   in simulateCells xys slice world

simulateSync :: World -> IO World
simulateSync old = return $
  runEval $ do
    let new = setCells old (simulate (Partition.fromWorld old) old)
    grown <- grow old

    return $ stack new grown

simulateAsync :: Int -> Int -> World -> IO World
simulateAsync sliceWidth sliceHeight old = return $
  runEval $ do
    let newWorld = setCells old (concatMap (`simulate` old) slices)
        partitionBorderCells =
          simulateCells
            (toList $ Partition.partitionBorders sliceWidth sliceHeight old)
            (Partition.fromWorld old)
            old
        newWorldWithBorder = setCells newWorld partitionBorderCells
    grown <- grow old
    return $ stack newWorldWithBorder grown
  where
    slices = Partition.partition sliceWidth sliceHeight old
