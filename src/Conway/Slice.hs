module Conway.Slice where

import Control.DeepSeq

data Slice = Slice {minX :: Int, maxX :: Int, minY :: Int, maxY :: Int}
  deriving (Show, Eq)

instance NFData Slice where
  rnf slice =
    rnf (minX slice)
      `seq` rnf (maxX slice)
      `seq` rnf (minY slice)
      `seq` rnf (maxY slice)

contains :: Slice -> (Int, Int) -> Bool
contains slice (x, y) = x >= minX slice && x <= maxX slice && y >= minY slice && y <= maxY slice
