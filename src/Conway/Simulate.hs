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

simulateCells :: Slice.Slice -> World -> [Vec2] -> [SimulateResult]
simulateCells slice oldWorld = map (simulateCell slice oldWorld)

grow :: World -> World
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
      growCellsTB = simulateCells growSlice world cellsTB
      growCellsLR = simulateCells growSlice world cellsLR
      growSize :: (Vec2, Bool) -> Int -> Int
      growSize (_, live) count = if live then count + 1 else count
      growSizeTB = foldr growSize 0 growCellsTB
      growSizeLR = foldr growSize 0 growCellsLR
      grownWorld =
        fromWH
          (if growSizeLR > 0 then width world + 2 else width world)
          (if growSizeTB > 0 then height world + 2 else height world)
   in setCells
        (setCells grownWorld (if growSizeLR > 0 then growCellsLR else []))
        (if growSizeTB > 0 then growCellsTB else [])

simulate :: Slice.Slice -> World -> [SimulateResult]
simulate slice world = do
  let xs = [(Slice.minX slice) .. (Slice.maxX slice)]
      ys = [(Slice.minY slice) .. (Slice.maxY slice)]
      xys = concatMap (\x -> map (x,) ys) xs
   in simulateCells slice world xys

simulateSync :: World -> World
simulateSync old =
  runEval $ do
    let new = setCells old (simulate (Partition.fromWorld old) old)
        grown = grow old

    return $ stack new grown

simulateAsync :: Int -> Int -> Int -> World -> World
simulateAsync sliceWidth sliceHeight chunkSize old = runEval $ do
  let slices = Partition.partition sliceWidth sliceHeight old
      partitionBorders = toList $ Partition.partitionBorders sliceWidth sliceHeight old

  (slices', partitionBorders') <- parTuple2 rdeepseq rdeepseq (slices, partitionBorders)

  let grown = grow old
      sliceCells = map (`simulate` old) slices'
      partitionBorderCells =
        simulateCells
          (Partition.fromWorld old)
          old
          partitionBorders'

  (grown', partitionBorderCells', sliceCells') <-
    parTuple3
      rdeepseq
      partitionBorderCellsStrategy
      sliceCellsStrategy
      (grown, partitionBorderCells, sliceCells)

  let sliceCellWorld = setCells old (concat sliceCells')

  return $ stack (setCells sliceCellWorld partitionBorderCells') grown'
  where
    sliceCellsStrategy :: Strategy [[SimulateResult]]
    sliceCellsStrategy = parList rdeepseq

    partitionBorderCellsStrategy :: Strategy [SimulateResult]
    partitionBorderCellsStrategy = parListChunk chunkSize rdeepseq
