module Helpers (canMove, canMove') where

import Data.Position
import Debug.Trace

canMove :: (Movable a, Movable b) => a -> [b] -> Bool
canMove a = all (canMove' dir' area')
  where area' = area a
        dir' = getDir a

canMove' :: (Movable a) => Direction -> [(Float, Float)] -> a -> Bool
canMove' d area' object = if (size c) > (size area')
                          then not (inObject d area' c)
                          else not (inObject d c area')
  where c = coordinates object

size [(x1,y1),(_,_),(x2,y2),(_,_)] = (x2 - x1) * (y2 - y1)

area :: (Movable a) => a -> [(Float, Float)]
area a = [(x'-w',y'-h'),(x'+w',y'-h'),(x'+w',y'+h'),(x'-w',y'+h')]
  where (Position x' y') = getPos a
        w' = (width a) / 2
        h' = (height a) / 2

inObject :: Direction -> [(Float, Float)] -> [(Float,Float)] -> Bool
inObject d [(a1,b1),(a2,b2),(a3,b3),(a4,b4)] [(x1,y1),(x2,y2),(x3,y3),(x4,y4)]
    | a1' >= x1 && a1' <= x3 && b1' >= y1 && b1' <= y3 ||
      a2' >= x1 && a2' <= x3 && b2' >= y1 && b2' <= y3 ||
      a3' >= x1 && a3' <= x3 && b3' >= y1 && b3' <= y3 ||
      a4' >= x1 && a4' <= x3 && b4' >= y1 && b4' <= y3 = True
  | otherwise = False
  where (a1',b1',a2',b2',a3',b3',a4',b4') = case d of
                       North -> (a1, b1 + 1,a2, b2 + 1,a3, b3 + 1,a4, b4 + 1)
                       West  -> (a1 - 1, b1,a2 - 1, b2,a3 - 1, b3,a4 - 1, b4)
                       South -> (a1, b1 - 1,a2, b2 - 1,a3, b3 - 1,a4, b4 - 1)
                       East  -> (a1 + 1, b1,a2 + 1, b2,a3 + 1, b3,a4 + 1, b4)
