{-# LANGUAGE DeriveGeneric #-}
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
  deriving (Show, Generic)

data BlockType = Road | Sidewalk | Building
  deriving (Show, Eq, Generic)

instance FromJSON BlockType

moveBlocks :: [Block] -> [BlockType] -> [Block]
moveBlocks xs t = filter f xs
  where f (Block _ _ _ x) = notElem x t

instance FromJSON Block
