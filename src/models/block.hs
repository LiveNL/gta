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

block :: [([Char],Picture)] -> Block -> Picture
block images block@(Block (Position x y) w h t s) = case t of
   Road     -> draw images block
   Wall     -> translate x y $ color (greyN 0.5) $ rectangleSolid 0 0
   Building -> draw images block
   Sidewalk -> draw images block
   Tree     -> draw images block
   Coin     -> draw images block
   _        -> translate x y $ rectangleSolid w h

