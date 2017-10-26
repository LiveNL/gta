module Traffic (car, person, block, updateCars, updatePeople) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Helpers

import Data.Block
import Data.Car
import Data.Position
import Data.Person
import Data.Game

car :: Car -> Picture
car (Car (Position x y) c) = translate x y $ color c $ rectangleSolid 20 30

person :: Person -> Picture
person (Person (Position x y) c _) = translate x y $ color c $ rectangleSolid 10 10

block :: Block -> Picture
block (Block (Position x y) w h t)
  | t == Road = translate x y $ color (greyN 0.5) $ rectangleSolid w h
  | otherwise = translate x y $ color white $ rectangleSolid w h

updateCars :: [Car] -> GTA -> GTA
updateCars cars game = game { cars = updateCars' }
  where updateCars' = map (updateCar game) cars

updateCar :: GTA -> Car -> Car
updateCar game car@(Car (Position x' y') c')
  | canMove (x', y') (world game) = newCarPosition car
  | otherwise = switchCarPosition car

newCarPosition :: Car -> Car
newCarPosition car@(Car (Position x' y') c')
  | x' == 0 = move (Position 0 1) car    -- TODO: change first x to Direction
  | x' == 1 = move (Position (-1) 0) car -- TODO: change first x to Direction
  | x' == 2 = move (Position 0 (-1)) car -- TODO: change first x to Direction
  | otherwise = move (Position 1 0) car

switchCarPosition :: Car -> Car
switchCarPosition car@(Car (Position x' y') c')
  | x' < 3 = Car { carPosition = Position { x = x', y = y' }, carColor = c' } -- TODO: change first x to Direction
  | otherwise = Car { carPosition = Position { x = x', y = y' }, carColor = c' }

updatePeople :: [Person] -> GTA -> GTA
updatePeople people game = game { people = updatePeople' }
  where updatePeople' = map (updatePerson game) people

updatePerson :: GTA -> Person -> Person
updatePerson game person@(Person (Position x y) _ d)
  | canMove (x, y, d) (world game) = newPersonPosition person
  | otherwise = switchPersonPosition person

newPersonPosition :: Person -> Person
newPersonPosition person@(Person _ _ d)
  | d == North = move (Position 0    1)  person
  | d == West  = move (Position (-1) 0)  person
  | d == South = move (Position 0  (-1)) person
  | otherwise  = move (Position 1    0)  person

switchPersonPosition :: Person -> Person
switchPersonPosition person@(Person (Position x' y') c d) = changeDir person
