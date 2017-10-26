module Traffic (car, person, block, updateCars) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Helpers

import Data.Block
import Data.Car
import Data.Position
import Data.Person
import Data.Game

car :: Car -> Picture
car (Car (Position x y _) c) = translate x y $ color c $ rectangleSolid 20 30

person :: Person -> Picture
person (Person (Position x y _) c) = translate x y $ color c $ rectangleSolid 10 10

block :: Block -> Picture
block (Block (Position x y _) w h _) = translate x y $ color white $ rectangleSolid w h

updateCars :: [Car] -> GTA -> GTA
updateCars cars game = game { cars = updateCars' }
  where updateCars' = map (updateCar game) cars

updateCar :: GTA -> Car -> Car
updateCar game (Car (Position x' y' z') c')
  | canMove (x', y') (map block (world game)) = newCarPosition
  | otherwise = Car { carPosition = Position { x = x', y = y', z = z' }, carColor = c' }
    where newCarPosition = Car { carPosition =
          Position { x = x', y = y' + 1, z = z' }, carColor = c' }
