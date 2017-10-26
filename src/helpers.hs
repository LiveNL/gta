module Helpers (canMove) where

import Graphics.Gloss.Interface.Pure.Game
import Data.Block
import Data.Position
import Debug.Trace

canMove :: (Float, Float, Direction) -> [Block] -> Bool
canMove (x,y,d) = all canMove'
  where canMove' block
          | (blockType block) == Road = True
          | d == North = not (inBlock (x, y + 1) (coordinates block))
          | d == West  = not (inBlock (x - 1, y) (coordinates block))
          | d == South = not (inBlock (x, y - 1) (coordinates block))
          | otherwise  = not (inBlock (x + 1, y) (coordinates block))

inBlock :: (Float, Float) -> [(Float,Float)] -> Bool
inBlock (x,y) [(x1,y1),(x2,y2)]
  | x >= x1 && x <= x2 && y >= y1 && y <= y2 = True
  | otherwise = False

coordinates :: Block -> [(Float,Float)]
coordinates (Block (Position x' y') w h t) = [(x' - w',y' - h'),(x' + w', y'+ h')]
  where w' = w / 2
        h' = h / 2
