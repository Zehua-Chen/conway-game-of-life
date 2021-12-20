module Conway.World where

import qualified Control.Monad as Monad
import qualified Data.HashMap.Strict as Map

type Grid = Map.HashMap (Int, Int) Bool

data World = World {width :: Int, height :: Int, grid :: Grid}
  deriving (Eq)

instance Show World where
  show world =
    concatMap
      ( \y ->
          concatMap
            ( \x -> case Map.lookup (x, y) (grid world) of
                Just cell -> if cell then "X " else ". "
                Nothing -> "? "
            )
            [minX $ width world .. maxX $ width world]
            ++ "\n"
      )
      (reverse [minY $ height world .. maxY $ height world])

-- | Make sure a world has odd height and odd width
guard :: World -> IO ()
guard world = do
  Monad.guard $ odd $ height world
  Monad.guard $ odd $ width world

-- | Convert a 2D bool list into a World
fromList :: [[Bool]] -> IO World
fromList rows =
  let h = length rows
      w = length $ head rows
   in do
        Monad.guard $ odd h
        Monad.guard $ odd w

        let g = forEachRow rows Map.empty (maxY h) w
         in do return World {width = w, height = h, grid = g}
  where
    forEachRow [] g _ _ = g
    forEachRow (r : rs) g y w =
      let newG = forEachCol r g (minX w) y
       in forEachRow rs newG (y - 1) w

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

liveCount :: World -> Int
liveCount world = foldr (\cell count -> if cell then count + 1 else count) (0 :: Int) (grid world)

neighbors :: Int -> Int -> [(Int, Int)]
neighbors x y =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1),
    (x + 1, y + 1),
    (x + 1, y - 1),
    (x - 1, y + 1),
    (x - 1, y - 1)
  ]

liveNeighbors :: Int -> Int -> Grid -> Int
liveNeighbors x y g =
  foldr
    ( \neighbor count ->
        case Map.lookup neighbor g of
          Just alive -> if alive then count + 1 else count
          Nothing -> count
    )
    0
    (neighbors x y)
