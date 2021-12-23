{-# LANGUAGE DeriveGeneric #-}

module Conway.Json (worldFromJson) where

import qualified Conway.World as World
import qualified Data.Aeson as Json
import GHC.Generics

data JsonCell = JsonCell
  { x :: Int,
    y :: Int
  }
  deriving (Generic, Show)

instance Json.FromJSON JsonCell

data JsonWorld = JsonWorld
  { width :: Int,
    height :: Int,
    livingCells :: [JsonCell]
  }
  deriving (Generic, Show)

instance Json.FromJSON JsonWorld

jsonWorldToWorld :: JsonWorld -> World.World
jsonWorldToWorld json = World.setCells deadWorld cellsToSet
  where
    deadWorld = World.fromWH (width json) (height json)
    cellsToSet :: [(World.Vec2, Bool)]
    cellsToSet = map (\cell -> ((x cell, y cell), True)) (livingCells json)

worldFromJson :: FilePath -> IO World.World
worldFromJson filepath = do
  decoded' <- decoded
  case decoded' of
    Just world -> do
      return $ jsonWorldToWorld world
    Nothing -> do
      return undefined
  where
    decoded :: IO (Maybe JsonWorld)
    decoded = Json.decodeFileStrict filepath
