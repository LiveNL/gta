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
    blockType     :: BlockType,
    blockSprite   :: Sprite }
  deriving (Show, Generic, FromJSON, ToJSON)

data BlockType = Road | Sidewalk | Building | Wall | Tree
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Movable Block where
  getPos Block{blockPosition} = Position (x blockPosition) (y blockPosition)

  width = blockWidth
  height = blockHeight

  getDir _ = North

  getSprite Block{blockSprite} = Sprite (spriteType blockSprite) (spriteState blockSprite)

moveBlocks :: [Block] -> [BlockType] -> [Block]
moveBlocks xs t = filter f xs
  where f (Block _ _ _ x _) = elem x t