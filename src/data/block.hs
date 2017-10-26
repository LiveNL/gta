module Data.Block where

import Graphics.Gloss
import Data.Position

data Block = Block
  { blockPosition :: Position,
    blockWidth    :: Float,
    blockHeight   :: Float,
    blockType     :: BlockType }
  deriving Show

data BlockType = Road | Sidewalk | Building
  deriving Show
