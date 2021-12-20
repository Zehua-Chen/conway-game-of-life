module Conway.World
  ( Grid,
    Vec2,
    World (World, width, height, grid),
    guard,
    fromList,
    fromWH,
    minX,
    maxX,
    minY,
    maxY,
    liveCount,
    neighbors,
    liveNeighbors,
    getCell,
    setCell,
    setCells,
    stack,
  )
where

import qualified Control.Monad as Monad
import qualified Data.HashMap.Strict as Map
import Data.Maybe

type Vec2 = (Int, Int)

type Grid = Map.HashMap Vec2 Bool

data World = World {width :: Int, height :: Int, grid :: Grid}
  deriving (Eq)

instance Show World where
  show world =
    "width = " ++ show (width world) ++ ", height = " ++ show (height world) ++ "\n"
      ++ concatMap
        ( \y ->
            concatMap
              ( \x -> case Map.lookup (x, y) (grid world) of
                  Just cell -> if cell then "X " else ". "
                  Nothing -> "? "
              )
              [minX world .. maxX world]
              ++ "\n"
        )
        (reverse [minY world .. maxY world])

-- | Make sure a world has odd height and odd width
guard :: World -> IO ()
guard world = do
  Monad.guard $ odd $ height world
  Monad.guard $ odd $ width world

fromWH :: Int -> Int -> World
fromWH w h = World {width = w, height = h, grid = Map.empty}

-- | Convert a 2D bool list into a World
fromList :: [[Bool]] -> IO World
fromList rows =
  do
    Monad.guard $ odd h
    Monad.guard $ odd w

    let g = forEachRow rows Map.empty (maxY emptyWorld)
     in do return World {width = w, height = h, grid = g}
  where
    forEachRow :: [[Bool]] -> Grid -> Int -> Grid
    forEachRow [] g _ = g
    forEachRow (r : rs) g y =
      let newG = forEachCol r g (minX emptyWorld) y
       in forEachRow rs newG (y - 1)

    forEachCol :: [Bool] -> Grid -> Int -> Int -> Grid
    forEachCol [] g _ _ = g
    forEachCol (c : cs) g x y =
      if c
        then forEachCol cs (Map.insert (x, y) True g) (x + 1) y
        else forEachCol cs (Map.insert (x, y) False g) (x + 1) y

    h = length rows
    w = length $ head rows
    emptyWorld = World {width = w, height = h, grid = Map.empty}

minX :: World -> Int
minX world = negate $ maxX world

maxX :: World -> Int
maxX world = width world `div` 2

minY :: World -> Int
minY world = negate $ maxY world

maxY :: World -> Int
maxY world = height world `div` 2

liveCount :: World -> Int
liveCount world = foldr (\cell count -> if cell then count + 1 else count) (0 :: Int) (grid world)

neighbors :: Int -> Int -> [Vec2]
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

liveNeighbors :: World -> Int -> Int -> Int
liveNeighbors world x y =
  foldr
    ( \neighbor count ->
        if getCell world neighbor
          then count + 1
          else count
    )
    0
    (neighbors x y)

getCell :: World -> Vec2 -> Bool
getCell world pos = fromMaybe False (Map.lookup pos (grid world))

setCell :: World -> Vec2 -> Bool -> World
setCell world pos cell =
  World
    { width = width world,
      height = height world,
      grid = Map.insert pos cell (grid world)
    }

setCells :: (Foldable m) => World -> m (Vec2, Bool) -> World
setCells = foldr (\(pos, v) w -> setCell w pos v)

-- | merge two world by layering a on top of b
stack :: World -> World -> World
stack a b =
  World
    { width = max (width a) (width b),
      height = max (height a) (height b),
      grid = Map.union (grid a) (grid b)
    }
