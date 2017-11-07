{-# LANGUAGE NamedFieldPuns #-}
module Traffic (block, updateCars, updatePeople) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Data.Maybe
import Data.Fixed (mod')

import Helpers

import Data.Block
import Data.Car
import Data.Position
import Data.Person
import Data.Game
import Debug.Trace

block :: [([Char],Picture)] -> Block -> Picture
block images block@(Block (Position x y) w h t s) = case t of
   Road -> draw images block
   Wall -> translate x y $ color (greyN 0.5) $ rectangleSolid 0 0
   Building -> draw images block
   Sidewalk -> draw images block
   Tree -> draw images block
   _ -> translate x y $ rectangleSolid w h

updateCars :: [Car] -> Int -> GTA -> GTA
updateCars cars rInt game = game { cars = updateCars', gameState = updateGameState }
  where update = map (updateCar game rInt) cars
        updateCars' = map snd update
        updateGameState | elem Dead (map fst update) = Dead
                        | otherwise = gameState game

updateCar :: GTA -> Int -> Car -> (GameState, Car)
updateCar game rInt car@Car{velocity} = case velocity of
  0 -> (Running, car)
  1 | canMove 4 car walls' -> (Running, (changeDirR rInt car))
    | canMove 1 car cars' -> (Running, changeDir 2 car)
--    | canMove 1 car cars' -> (Running, car)
    | canMove 1 (player game) [car] && (playerState (player game) == Walking ) -> (Dead, car)
    | canMove 1 car [(player game)] -> (Running, car)
    | canMove 4 car blocks' -> (Running, newCarPosition car)
    | otherwise -> (Running, (changeDir rInt car))
         where blocks' = moveBlocks (blocks game) [Road]
               walls' = moveBlocks (blocks game) [Wall]
               carIndex = fromJust (elemIndex car (cars game))
               cars' = take carIndex (cars game) ++ drop (1 + carIndex) (cars game)

newCarPosition :: Car -> Car
newCarPosition car@(Car (Position _ _) _ d _) = case d of
  North -> move (Position 0    1)  car
  West  -> move (Position (-1) 0)  car
  South -> move (Position 0  (-1)) car
  _     -> move (Position 1    0)  car

updatePeople :: [Person] -> Int -> GTA -> GTA
updatePeople people rInt game = game { people = updatePeople' }
  where updatePeople' = map (updatePerson game rInt) people

updatePerson :: GTA -> Int -> Person -> Person
updatePerson game rInt person
  | personVelocity person == 0 = person
  | canMove 1 person (cars game) = changeDir rInt person
  | canMove 1 person people' = changeDirR rInt person
  | canMove 1 person [player game] = changeDirR rInt person
  | canMove 4 person blocks' = newPersonPosition person { personSprite = Sprite { spriteType = spriteType (personSprite person), spriteState = sprite' }}
  | otherwise = changeDirR rInt person
    where blocks' = moveBlocks (blocks game) [Sidewalk]
          people' = take personIndex (people game) ++ drop (1 + personIndex) (people game)
          personIndex = fromJust (elemIndex person (people game))
          sprite' | mod' (roundDecimals (elapsedTime game) 2) 0.5 == 0 = nextSprite (personSprite person)
                  | otherwise = spriteState (personSprite person)

newPersonPosition :: Person -> Person
newPersonPosition person@(Person _ _ d _)
  | d == North = move (Position 0    1)  person
  | d == West  = move (Position (-1) 0)  person
  | d == South = move (Position 0  (-1)) person
  | otherwise  = move (Position 1    0)  person
