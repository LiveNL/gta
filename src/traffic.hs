{-# LANGUAGE NamedFieldPuns #-}
module Traffic (car, person, block, updateCars, updatePeople) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Helpers

import Data.Block
import Data.Car
import Data.Position
import Data.Person
import Data.Game

import Debug.Trace

car :: Car -> Picture
car car@(Car (Position x y) c d _) = translate x y $ color c $ rectangleSolid w' h'
        w' = width car
        h' = height car

person :: Person -> Picture
person (Person (Position x y) c _) = translate x y $ color c $ rectangleSolid 10 10

block :: Block -> Picture
block (Block (Position x y) w h t)
  | t == Road = translate x y $ color (greyN 0.5) $ rectangleSolid w h
  | t == Building = translate x y $ color (dark red) $ rectangleSolid w h
  | otherwise = translate x y $ color (greyN 0.7) $ rectangleSolid w h

updateCars :: [Car] -> GTA -> Int -> GTA
updateCars cars game rInt = game { cars = updateCars' }
  where updateCars' = map (updateCar game rInt) cars

updateCar :: GTA -> Int -> Car -> Car
updateCar game rInt car@Car{velocity} = case velocity of
  0 -> car
  1 -> if canMove car blocks' && canMove car [player game] then newCarPosition car
                                                           else changeDir car rInt
         where blocks' = moveBlocks (blocks game) [Road]

newCarPosition :: Car -> Car
newCarPosition car@(Car (Position _ _) _ d _) = case d of
  North -> move (Position 0    1)  car
  West  -> move (Position (-1) 0)  car
  South -> move (Position 0  (-1)) car
  _     -> move (Position 1    0)  car

updatePeople :: [Person] -> GTA -> GTA
updatePeople people game = game { people = updatePeople' }
  where updatePeople' = map (updatePerson game) people

updatePerson :: GTA -> Person -> Person
updatePerson game person
  | canMove person blocks' = newPersonPosition person
  | otherwise = changeDir person 0
    where blocks' = moveBlocks (blocks game) [Sidewalk]

newPersonPosition :: Person -> Person
newPersonPosition person@(Person _ _ d)
  | d == North = move (Position 0    1)  person
  | d == West  = move (Position (-1) 0)  person
  | d == South = move (Position 0  (-1)) person
  | otherwise  = move (Position 1    0)  person
