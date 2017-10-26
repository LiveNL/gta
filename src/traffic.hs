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
car (Car (Position x y _) c) = translate x y $ color c $ rectangleSolid 20 30

person :: Person -> Picture
person (Person (Position x y _) c) = translate x y $ color c $ rectangleSolid 10 10

block :: Block -> Picture
block (Block (Position x y _) w h t)
  | t == Road = translate x y $ color (greyN 0.5) $ rectangleSolid w h
  | otherwise = translate x y $ color white $ rectangleSolid w h

updateCars :: [Car] -> GTA -> GTA
updateCars cars game = game { cars = updateCars' }
  where updateCars' = map (updateCar game) cars

updateCar :: GTA -> Car -> Car
updateCar game car@(Car (Position x' y' z') c')
  | canMove (x', y', z') (world game) = newCarPosition car
  | otherwise = switchCarPosition car

newCarPosition :: Car -> Car
newCarPosition car@(Car (Position x' y' z') c')
  | z' == 0 = Car { carPosition = Position { x = x', y = y' + 1, z = z' }, carColor = c' }
  | z' == 1 = Car { carPosition = Position { x = x' - 1, y = y', z = z' }, carColor = c' }
  | z' == 2 = Car { carPosition = Position { x = x', y = y' - 1, z = z' }, carColor = c' }
  | otherwise = Car { carPosition = Position { x = x' + 1, y = y', z = z' }, carColor = c' }

switchCarPosition :: Car -> Car
switchCarPosition car@(Car (Position x' y' z') c')
  | z' < 3 = Car { carPosition = Position { x = x', y = y', z = z' + 1 }, carColor = c' }
  | otherwise = Car { carPosition = Position { x = x', y = y', z = 0 }, carColor = c' }

updatePeople :: [Person] -> GTA -> GTA
updatePeople people game = game { people = updatePeople' }
  where updatePeople' = map (updatePerson game) people

updatePerson :: GTA -> Person -> Person
updatePerson game person@(Person (Position x' y' z') c')
  | canMove (x', y', z') (world game) = newPersonPosition person
  | otherwise = switchPersonPosition person

newPersonPosition :: Person -> Person
newPersonPosition person@(Person (Position x' y' z') c')
  | z' == 0 = Person { personPosition = Position { x = x', y = y' + 1, z = z' }, personColor = c' }
  | z' == 1 = Person { personPosition = Position { x = x' - 1, y = y', z = z' }, personColor = c' }
  | z' == 2 = Person { personPosition = Position { x = x', y = y' - 1, z = z' }, personColor = c' }
  | otherwise = Person { personPosition = Position { x = x' + 1, y = y', z = z' }, personColor = c' }

switchPersonPosition :: Person -> Person
switchPersonPosition person@(Person (Position x' y' z') c')
  | z' < 3 = Person { personPosition = Position { x = x', y = y', z = z' + 1 }, personColor = c' }
  | otherwise = Person { personPosition = Position { x = x', y = y', z = 0 }, personColor = c' }
