module Helpers (canMove, canMove') where

import Data.Position
import Debug.Trace

canMove :: (Movable a, Movable b) => a -> [b] -> Bool
canMove a = all (canMove' dir' area')
  where area' = coordinates a
        dir' = getDir a

canMove' :: (Movable a) => Direction -> [(Float, Float)] -> a -> Bool
canMove' d area' object = if (size c) > (size area')
                          then not (inObject d area' c)
                          else not (inObject d c area')
  where c = coordinates object

size [(x1,y1),(_,_),(x2,y2),(_,_)] = (x2 - x1) * (y2 - y1)

inObject :: Direction -> [(Float, Float)] -> [(Float,Float)] -> Bool
inObject d xs [(x1,y1),_,(x3,y3),_] = or (map (checkObject d [x1,x3,y1,y3]) xs)

checkObject :: Direction -> [Float] -> (Float,Float) -> Bool
checkObject d [x1,x3,y1,y3] (a1,b1) = a1' >= x1 && a1' <= x3 && b1' >= y1 && b1' <= y3
  where (a1',b1') = case d of
                      North -> (a1, b1 + 1)
                      West  -> (a1 - 1, b1)
                      South -> (a1, b1 - 1)
                      East  -> (a1 + 1, b1)
