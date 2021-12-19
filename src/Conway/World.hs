module Conway.World where

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
