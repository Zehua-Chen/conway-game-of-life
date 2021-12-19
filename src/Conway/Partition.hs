{-# LANGUAGE TupleSections #-}

module Conway.Partition where

import qualified Conway.World as World

data Slice = Slice {minX :: Int, maxX :: Int, minY :: Int, maxY :: Int}
  deriving (Show, Eq)

partition :: Int -> Int -> World.World -> [Slice]
partition sliceWidth sliceHeight world =
  map
    ( \((xMin, xMax), (yMin, yMax)) ->
        Slice {minX = xMin, maxX = xMax, minY = yMin, maxY = yMax}
    )
    xys
  where
    slice :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
    slice current end step slices
      | current > end = slices
      | otherwise = slice (current + step) end step ((current, current + step - 1) : slices)

    xs = slice (World.minX $ World.width world) (World.maxX $ World.width world) sliceWidth []
    ys = slice (World.minY $ World.height world) (World.maxY $ World.height world) sliceHeight []

    xys :: [((Int, Int), (Int, Int))]
    xys = concatMap (\x -> map (x,) ys) xs
