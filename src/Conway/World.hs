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
import Data.Array ((!), (//))
import qualified Data.Array as Array

type Vec2 = (Int, Int)

type Grid = Array.Array Vec2 Bool

data World = World {width :: Int, height :: Int, grid :: Grid}
  deriving (Eq)

instance Show World where
  show world =
    "width = " ++ show (width world) ++ ", height = " ++ show (height world) ++ "\n"
      ++ concatMap
        ( \y ->
            concatMap
              ( \x -> if grid world ! (x, y) then "X " else ". "
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
fromWH w h =
  World
    { width = w,
      height = h,
      grid = Array.array ((minXFromW w, minYFromH h), (maxXFromW w, maxYFromH h)) xys
    }
  where
    xs = [(minXFromW w) .. (maxXFromW w)]
    ys = [(minYFromH h) .. (maxYFromH h)]
    xys = concatMap (\x -> map (\y -> ((x, y), False)) ys) xs

-- | Convert a 2D bool list into a World
fromList :: [[Bool]] -> IO World
fromList rows =
  do
    Monad.guard $ odd h
    Monad.guard $ odd w

    return $ forEachRow rows emptyWorld (maxYFromH h)
  where
    forEachRow :: [[Bool]] -> World -> Int -> World
    forEachRow [] world _ = world
    forEachRow (r : rs) world y =
      let newWorld = forEachCol r world (minX emptyWorld) y
       in forEachRow rs newWorld (y - 1)

    forEachCol :: [Bool] -> World -> Int -> Int -> World
    forEachCol [] world _ _ = world
    forEachCol (c : cs) world x y = forEachCol cs (setCell world (x, y) c) (x + 1) y

    h = length rows
    w = length $ head rows
    emptyWorld = fromWH w h

maxXFromW :: Int -> Int
maxXFromW w = w `div` 2

minXFromW :: Int -> Int
minXFromW w = negate $ maxXFromW w

maxYFromH :: Int -> Int
maxYFromH h = h `div` 2

minYFromH :: Int -> Int
minYFromH h = negate $ maxYFromH h

minX :: World -> Int
minX world = minXFromW (width world)

maxX :: World -> Int
maxX world = maxXFromW (width world)

minY :: World -> Int
minY world = minYFromH (height world)

maxY :: World -> Int
maxY world = maxYFromH (height world)

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
getCell world pos@(x, y) =
  (x >= minX world && x <= maxX world && y >= minY world && y <= maxY world)
    && (grid world ! pos)

setCell :: World -> Vec2 -> Bool -> World
setCell world pos cell =
  World
    { width = width world,
      height = height world,
      grid = grid world // [(pos, cell)]
    }

setCells :: (Foldable m) => World -> m (Vec2, Bool) -> World
setCells = foldr (\(pos, v) w -> setCell w pos v)

-- | merge two world by layering a on top of b
stack :: World -> World -> World
stack a b = withA
  where
    biggerWidth = max (width a) (width b)
    biggerHeight = max (height a) (height b)

    emptyWorld = fromWH biggerWidth biggerHeight

    withB = setCells emptyWorld (map (\i -> (i, getCell b i)) (Array.indices (grid b)))
    withA = setCells withB (map (\i -> (i, getCell a i)) (Array.indices (grid a)))
