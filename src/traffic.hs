module Traffic (car, person, block, updateCars, updatePeople) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Helpers

import Data.Block
import Data.Car
import Data.Position
import Data.Person
import Data.Game

car :: Car -> Picture
car (Car (Position x y) c _) = translate x y $ color c $ rectangleSolid 20 30

person :: Person -> Picture
person (Person (Position x y) c _) = translate x y $ color c $ rectangleSolid 10 10

block :: Block -> Picture
block (Block (Position x y) w h t)
  | t == Road = translate x y $ color (greyN 0.5) $ rectangleSolid w h
  | t == Building = translate x y $ color (dark red) $ rectangleSolid w h
  | otherwise = translate x y $ color (greyN 0.7) $ rectangleSolid w h

updateCars :: [Car] -> GTA -> GTA
updateCars cars game = game { cars = updateCars' }
  where updateCars' = map (updateCar game) cars

updateCar :: GTA -> Car -> Car
updateCar game car@(Car (Position x y) _ d)
  | canMove (x, y, d) blocks' = newCarPosition car
  | otherwise = switchCarPosition car
    where blocks' = moveBlocks (blocks game) [Road]

newCarPosition :: Car -> Car
newCarPosition car@(Car (Position _ _) _ d)
  | d == North = move (Position 0    1)  car
  | d == West  = move (Position (-1) 0)  car
  | d == South = move (Position 0  (-1)) car
  | otherwise  = move (Position 1    0)  car

switchCarPosition :: Car -> Car
switchCarPosition car@(Car (Position x' y') c d) = changeDir car

updatePeople :: [Person] -> GTA -> GTA
updatePeople people game = game { people = updatePeople' }
  where updatePeople' = map (updatePerson game) people

updatePerson :: GTA -> Person -> Person
updatePerson game person@(Person (Position x y) _ d)
  | canMove (x, y, d) blocks' = newPersonPosition person
  | otherwise = switchPersonPosition person
    where blocks' = moveBlocks (blocks game) [Sidewalk]

newPersonPosition :: Person -> Person
newPersonPosition person@(Person _ _ d)
  | d == North = move (Position 0    1)  person
  | d == West  = move (Position (-1) 0)  person
  | d == South = move (Position 0  (-1)) person
  | otherwise  = move (Position 1    0)  person

switchPersonPosition :: Person -> Person
switchPersonPosition person@(Person (Position x' y') c d) = changeDir person
