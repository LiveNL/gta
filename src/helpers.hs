module Helpers (canMove) where

import Data.Position
import Data.List

canMove :: (Movable a, Movable b) => Int -> a -> [b] -> Bool
canMove i a blocks = pointsAmount >= i
  where pointsAmount  = length (elemIndices True pointsInBlock)
        pointsInBlock = concat (map (canMove' (getDir a) (coordinates a)) blocks)

canMove' :: (Movable a) => Direction -> [(Float, Float)] -> a -> [Bool]
canMove' dir crds block = inObject dir crds (coordinates block)

inObject :: Direction -> [(Float,Float)] -> [(Float,Float)] -> [Bool]
inObject d crds [(x1,y1),_,(x3,y3),_] = map (checkObject d [x1,x3,y1,y3]) crds

checkObject :: Direction -> [Float] -> (Float,Float) -> Bool
checkObject d [x1,x3,y1,y3] (a1,b1) = a1' >= x1 && a1' <= x3 && b1' >= y1 && b1' <= y3
  where (a1',b1') = case d of
                      North -> (a1, b1 + 1)
                      West  -> (a1 - 1, b1)
                      South -> (a1, b1 - 1)
                      East  -> (a1 + 1, b1)
