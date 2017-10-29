module Helpers (canMove) where

import Data.Position

canMove :: (Movable a, Movable b) => a -> [b] -> Bool
canMove a = all (canMove' dir' area')
  where area' = area a
        dir' = getDir a

canMove' :: (Movable a) => Direction -> [(Float, Float)] -> a -> Bool
canMove' d xs object = all can' xs
  where can' (x,y) = case d of
                       North -> not (inObject (x, y + 1) (coordinates object))
                       West  -> not (inObject (x - 1, y) (coordinates object))
                       South -> not (inObject (x, y - 1) (coordinates object))
                       _     -> not (inObject (x + 1, y) (coordinates object))

area :: (Movable a) => a -> [(Float, Float)]
area a = [(x'-w',y'-h'),(x'+w',y'-h'),(x'+w',y'+h'),(x'-w',y'+h')]
  where (Position x' y') = getPos a
        w' = (width a) / 2
        h' = (height a) / 2

inObject :: (Float, Float) -> [(Float,Float)] -> Bool
inObject (x,y) [(x1,y1),(x2,y2)]
  | x >= x1 && x <= x2 && y >= y1 && y <= y2 = True
  | otherwise = False
