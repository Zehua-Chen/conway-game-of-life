module Conway.PPM (save) where

import Control.Monad (forM_)
import qualified Conway.World as World
import System.IO

cellToPixel :: Bool -> String
cellToPixel cell = if cell then "1" else "0"

save :: World.World -> FilePath -> IO ()
save world filename = do
  withFile
    filename
    WriteMode
    ( \handle -> do
        hPutStrLn handle "P1"
        hPutStrLn handle (show (World.width world) ++ " " ++ show (World.height world))

        forM_
          (reverse [World.minY world .. World.maxY world])
          ( \y -> do
              forM_
                [World.minX world .. World.maxX world]
                ( \x -> do
                    hPutStr handle (cellToPixel $ World.getCell world (x, y))
                )

              hPutStrLn handle ""
          )
    )
