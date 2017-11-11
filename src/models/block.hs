{-# LANGUAGE DeriveGeneric, NamedFieldPuns, DeriveAnyClass #-}
module Models.Block where

import Data.Aeson
import GHC.Generics
import Graphics.Gloss

import Models.Position

data Block = Block
  { blockPosition :: Position,
    blockWidth    :: Float,
    blockHeight   :: Float,
    blockType     :: BlockType,
    blockSprite   :: Sprite }
  deriving (Show, Generic, FromJSON, ToJSON)

data BlockType = Road | Sidewalk | Building | Wall | Tree | Coin
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Movable Block where
  getPos    Block{blockPosition} = Position (x blockPosition) (y blockPosition)
  getSprite Block{blockSprite}   = Sprite (spriteType blockSprite) (spriteState blockSprite)

  width    = blockWidth
  height   = blockHeight
  getDir _ = North


moveBlocks :: [Block] -> [BlockType] -> [Block]
moveBlocks xs t = filter f xs
  where f (Block _ _ _ x _) = elem x t
