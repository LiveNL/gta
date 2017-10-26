module Helpers (canMove) where

import Graphics.Gloss.Interface.Pure.Game
import Data.Block
import Data.Position
import Debug.Trace

canMove :: (Float, Float, Float) -> [Block] -> Bool
canMove (x,y,z) = all canMove'
  where canMove' block
          | (blockType block) == Road = True
          | z == 0 = not (inBlock (x, y + 1) (coordinates block))
          | z == 1 = not (inBlock (x - 1, y) (coordinates block))
          | z == 2 = not (inBlock (x, y - 1) (coordinates block))
          | otherwise = not (inBlock (x + 1, y) (coordinates block))

inBlock :: (Float, Float) -> [(Float,Float)] -> Bool
inBlock (x,y) [(x1,y1),(x2,y2)]
  | x >= x1 && x <= x2 && y >= y1 && y <= y2 = True
  | otherwise = False

coordinates :: Block -> [(Float,Float)]
coordinates (Block (Position x' y' _) w h t) = [(x' - w',y' - h'),(x' + w', y'+ h')]
  where w' = w / 2
        h' = h / 2
