{-# LANGUAGE DeriveGeneric, NamedFieldPuns, DeriveAnyClass #-}
module Data.Block where

import Data.Aeson
import Graphics.Gloss
import Data.Position
import GHC.Generics

data Block = Block
  { blockPosition :: Position,
    blockWidth    :: Float,
    blockHeight   :: Float,
    blockType     :: BlockType }
  deriving (Show, Generic, FromJSON, ToJSON)

data BlockType = Road | Sidewalk | Building | Wall
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Movable Block where
  getPos Block{blockPosition} = Position (x blockPosition) (y blockPosition)

  width b = blockWidth b
  height b = blockHeight b

  getDir _ = North

  getSprite b = Sprite sprite 1
    where sprite = case blockType b of
                  Road -> "road"
                  Sidewalk -> "sidewalk"

moveBlocks :: [Block] -> [BlockType] -> [Block]
moveBlocks xs t = filter f xs
  where f (Block _ _ _ x) = elem x t