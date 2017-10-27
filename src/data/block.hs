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
  deriving (Show, Eq)

moveBlocks :: [Block] -> [BlockType] -> [Block]
moveBlocks xs t = filter f xs
  where f (Block _ _ _ x) = notElem x t
