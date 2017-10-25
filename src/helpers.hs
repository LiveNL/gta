module Helpers (canMove) where

import Graphics.Gloss.Interface.Pure.Game

canMove :: (Float, Float) -> [Picture] -> Bool
canMove (x,y) = all canMove'
  where canMove' block = not (inBlock (x,y) (coordinates block))

inBlock :: (Float, Float) -> Path -> Bool
inBlock (x,y) [(x1,y1), _, (x2,y2), _]
  | x >= x1 && x <= x2 && y >= y1 && y <= y2 = True
  | otherwise = False

coordinates :: Picture -> Path
coordinates (Translate x y color) = color' x y color
  where color' x y (Color _ polygon) = polygon' x y polygon
        polygon' x y (Polygon xs) = map (f x y) xs
        f x y z = (fst z + x, snd z + y)
