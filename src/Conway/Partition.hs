{-# LANGUAGE TupleSections #-}

module Conway.Partition
  ( Slice,
    minX,
    maxX,
    minY,
    maxY,
    partition,
    partitionBorders,
    fromWorld,
  )
where

import qualified Conway.World as World
import qualified Data.HashSet as Set

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

-- | Given slice width adn slice height, return a set of cells that are
-- on the borders of partition
partitionBorders :: Int -> Int -> World.World -> Set.HashSet (Int, Int)
partitionBorders sliceWidth sliceHeight world =
  Set.union
    (vertical (World.minX (World.width world) + (sliceWidth - 1)) Set.empty)
    (horizontal (World.minY (World.height world) + (sliceHeight - 1)) Set.empty)
  where
    vertical :: Int -> Set.HashSet (Int, Int) -> Set.HashSet (Int, Int)
    vertical x items
      | x >= World.maxX (World.width world) = items
      | otherwise = vertical (x + sliceWidth) right
      where
        left = foldr (\y current -> Set.insert (x, y) current) items ys
        right =
          if x + 1 > World.maxX (World.width world)
            then left
            else foldr (\y current -> Set.insert (x + 1, y) current) left ys

    horizontal :: Int -> Set.HashSet (Int, Int) -> Set.HashSet (Int, Int)
    horizontal y items
      | y >= World.maxY (World.height world) = items
      | otherwise = horizontal (y + sliceHeight) above
      where
        below = foldr (\x current -> Set.insert (x, y) current) items xs
        above =
          if y + 1 > World.maxY (World.height world)
            then below
            else foldr (\x current -> Set.insert (x, y + 1) current) below xs

    ys = [World.minY $ World.height world .. World.maxY $ World.height world]
    xs = [World.minX $ World.width world .. World.maxX $ World.width world]

fromWorld :: World.World -> Slice
fromWorld world =
  Slice
    { minX = World.minX $ World.width world,
      maxX = World.maxX $ World.width world,
      minY = World.minY $ World.height world,
      maxY = World.maxY $ World.height world
    }
