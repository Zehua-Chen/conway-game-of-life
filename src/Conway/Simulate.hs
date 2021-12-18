{-# LANGUAGE TupleSections #-}

module Conway.Simulate where

import Control.Monad (guard)
import qualified Data.HashMap.Strict as Map

type Grid = Map.HashMap (Int, Int) Bool

data World = World {width :: Int, height :: Int, grid :: Grid}
  deriving (Eq, Show)

fromList :: [[Bool]] -> IO World
fromList rows =
  let h = length rows
      w = length $ head rows
   in do
        guard $ odd h
        guard $ odd w

        let g = forEachRow rows Map.empty (minY h) w
         in do return World {width = w, height = h, grid = g}
  where
    forEachRow [] g _ _ = g
    forEachRow (r : rs) g y w =
      let newG = forEachCol r g (minX w) y
       in forEachRow rs newG (y + 1) w

    forEachCol [] g _ _ = g
    forEachCol (c : cs) g x y =
      if c
        then forEachCol cs (Map.insert (x, y) True g) (x + 1) y
        else forEachCol cs (Map.insert (x, y) False g) (x + 1) y

minX :: Int -> Int
minX w = negate $ maxX w

maxX :: Int -> Int
maxX w = w `div` 2

minY :: Int -> Int
minY h = negate $ maxX h

maxY :: Int -> Int
maxY h = h `div` 2

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
