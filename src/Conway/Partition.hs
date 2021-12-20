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
    slice :: Int -> Int -> Int -> [World.Vec2] -> [World.Vec2]
    slice current end step slices
      | current > end = slices
      | otherwise = slice (current + step) end step ((current, min (current + step - 1) end) : slices)

    xs = slice (World.minX world) (World.maxX world) sliceWidth []
    ys = slice (World.minY world) (World.maxY world) sliceHeight []

    xys :: [(World.Vec2, World.Vec2)]
    xys = concatMap (\x -> map (x,) ys) xs

-- | Given slice width adn slice height, return a set of cells that are
-- on the borders of partition
partitionBorders :: Int -> Int -> World.World -> Set.HashSet (Int, Int)
partitionBorders sliceWidth sliceHeight world =
  Set.union
    (vertical (World.minX world + (sliceWidth - 1)) Set.empty)
    (horizontal (World.minY world + (sliceHeight - 1)) Set.empty)
  where
    vertical :: Int -> Set.HashSet (Int, Int) -> Set.HashSet (Int, Int)
    vertical x items
      | x >= World.maxX world = items
      | otherwise = vertical (x + sliceWidth) right
      where
        left = foldr (\y current -> Set.insert (x, y) current) items ys
        right =
          if x + 1 > World.maxX world
            then left
            else foldr (\y current -> Set.insert (x + 1, y) current) left ys

    horizontal :: Int -> Set.HashSet (Int, Int) -> Set.HashSet (Int, Int)
    horizontal y items
      | y >= World.maxY world = items
      | otherwise = horizontal (y + sliceHeight) above
      where
        below = foldr (\x current -> Set.insert (x, y) current) items xs
        above =
          if y + 1 > World.maxY world
            then below
            else foldr (\x current -> Set.insert (x, y + 1) current) below xs

    ys = [World.minY world .. World.maxY world]
    xs = [World.minX world .. World.maxX world]

fromWorld :: World.World -> Slice
fromWorld world =
  Slice
    { minX = World.minX world,
      maxX = World.maxX world,
      minY = World.minY world,
      maxY = World.maxY world
    }
