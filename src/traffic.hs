module Traffic (car, person, blocks, updateCars) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Helpers

import Data.Car
import Data.Position
import Data.Person
import Data.Game

blocks :: [Picture]
blocks = [block, block1]

car :: Car -> Picture
car (Car (Position x y _) c) = translate x y $ color c $ rectangleSolid 20 30

person :: Person -> Picture
person (Person (Position x y _) c) = translate x y $ color c $ rectangleSolid 10 10

block :: Picture
block = translate (-15) 0 $ color white $ rectangleSolid 10 100
block1 = translate 0 100 $ color white $ rectangleSolid 100 10

updateCars :: [Car] -> GTA -> GTA
updateCars cars game = game { cars = updateCars' }
  where updateCars' = map updateCar cars

updateCar :: Car -> Car
updateCar (Car (Position x' y' z') c')
  | canMove (x', y') blocks = newCarPosition
  | otherwise = Car { carPosition = Position { x = x', y = y', z = z' }, carColor = c' }
    where newCarPosition = Car { carPosition =
          Position { x = x', y = y' + 1, z = z' }, carColor = c' }
