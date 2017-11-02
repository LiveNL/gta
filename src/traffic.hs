{-# LANGUAGE NamedFieldPuns #-}
module Traffic (car, person, block, updateCars, updatePeople) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Data.Maybe
import Data.Fixed (mod')
import Debug.Trace

import Helpers

import Data.Block
import Data.Car
import Data.Position
import Data.Person
import Data.Game

import Debug.Trace

car :: Car -> IO Picture
car (Car (Position x y) s d _) = do image@(Bitmap width height _ _) <- loadBMP ("./sprites/" ++ show (spriteType s) ++ "_" ++ show (spriteState s) ++ ".bmp")
                                    return (translate x y $ scale (30 / fromIntegral(height)) (20 / fromIntegral(width)) $ rotate angle $ image)
                                    where angle = case d of
                                                     North -> 0
                                                     West  -> 270
                                                     South -> 180
                                                     East  -> 90

person :: Person -> IO Picture
person (Person (Position x y) s d) = do image@(Bitmap width height _ _) <- loadBMP ("./sprites/" ++ show (spriteType s) ++ "_" ++ show (spriteState s) ++ ".bmp")
                                        return (translate x y $ scale (10 / fromIntegral(height)) (10 / fromIntegral(width)) $ rotate angle $ image)
                                        where angle = case d of
                                                         North -> 0
                                                         West  -> 270
                                                         South -> 180
                                                         East  -> 90

block :: Block -> Picture
block (Block (Position x y) w h t)
  | t == Road = translate x y $ color (greyN 0.5) $ rectangleSolid w h
  | t == Building = translate x y $ color (dark red) $ rectangleSolid w h
  | otherwise = translate x y $ color (greyN 0.7) $ rectangleSolid w h

updateCars :: [Car] -> Int -> GTA -> GTA
updateCars cars rInt game = game { cars = updateCars' }
  where updateCars' = map (updateCar game rInt) cars

updateCar :: GTA -> Int -> Car -> Car
updateCar game rInt car@Car{velocity} = case velocity of
  0 -> car
  1 | canMove 4 car walls'  -> newCarPosition $ changeDir rInt car
    | canMove 1 car cars' || canMove 1 (player game) [car] -> car -- collision
    | canMove 4 car blocks' -> newCarPosition car
    | otherwise -> newCarPosition $ changeDir 0 car
         where blocks' = moveBlocks (blocks game) [Road]
               walls' = moveBlocks (blocks game) [Wall]
               carIndex = fromJust (elemIndex car (cars game))
               cars' = take carIndex (cars game) ++ drop (1 + carIndex) (cars game)
               cdir = getDir car

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
updatePerson game person
  | canMove 4 person blocks' = newPersonPosition person { personSprite = Sprite { spriteType = spriteType (personSprite person),  spriteState = sprite' }}
  | otherwise = changeDir rInt person
    where blocks' = moveBlocks (blocks game) [Sidewalk]
          sprite' | mod' (roundDecimals (elapsedTime game) 2) 0.5 == 0 = nextWalking (personSprite person)
                  | otherwise = spriteState (personSprite person)

newPersonPosition :: Person -> Person
newPersonPosition person@(Person _ _ d)
  | d == North = move (Position 0    1)  person
  | d == West  = move (Position (-1) 0)  person
  | d == South = move (Position 0  (-1)) person
  | otherwise  = move (Position 1    0)  person
