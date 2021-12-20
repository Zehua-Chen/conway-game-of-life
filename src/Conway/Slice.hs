module Conway.Slice where

data Slice = Slice {minX :: Int, maxX :: Int, minY :: Int, maxY :: Int}
  deriving (Show, Eq)

contains :: Slice -> (Int, Int) -> Bool
contains slice (x, y) = x >= minX slice && x <= maxX slice && y >= minY slice && y <= maxY slice
