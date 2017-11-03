{-# LANGUAGE NamedFieldPuns #-}
module Traffic (car, person, block, updateCars, updatePeople) where

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

car :: [([Char],Picture)] -> Car -> Picture
car images car@(Car (Position x y) s d _) = translate x y $ scale scaleX scaleY $ rotate angle $ image
  where angle = case d of
                   North -> 0
                   West  -> 270
                   South -> 180
                   East  -> 90
        scaleX | d == North || d == South = (20 / fromIntegral(w'))
               | otherwise                = (30 / fromIntegral(h'))

        scaleY | d == North || d == South = (30 / fromIntegral(h'))
               | otherwise                = (20 / fromIntegral(w'))
        image@(Bitmap w' h' _ _) = fromJust (lookup name images)
        name = "./sprites/" ++ show (spriteType s) ++ "_" ++ show (spriteState s) ++ ".bmp"

person :: [([Char],Picture)] -> Person -> Picture
person images (Person (Position x y) s d) = (translate x y $ scale scaleX scaleY $ rotate angle $ image)
  where angle = case d of
                  North -> 0
                  West  -> 270
                  South -> 180
                  East  -> 90

        scaleX | d == North || d == South = (10 / fromIntegral(w'))
               | otherwise                = (10 / fromIntegral(h'))

        scaleY | d == North || d == South = (10 / fromIntegral(h'))
               | otherwise                = (10 / fromIntegral(w'))

        image@(Bitmap w' h' _ _) = fromJust (lookup name images)
        name = "./sprites/" ++ show (spriteType s) ++ "_" ++ show (spriteState s) ++ ".bmp"

block :: Block -> Picture
block (Block (Position x y) w h t) = case t of
   Road -> translate x y $ color (greyN 0.5) $ rectangleSolid w h
   Wall -> translate x y $ color (greyN 0.5) $ rectangleSolid w h
   Building -> translate x y $ color (dark red) $ rectangleSolid w h
   Sidewalk -> translate x y $ color (greyN 0.7) $ rectangleSolid w h
   _ -> translate x y $ color (greyN 0.7) $ rectangleSolid w h

updateCars :: [Car] -> Int -> GTA -> GTA
updateCars cars rInt game = game { cars = updateCars' }
  where updateCars' = map (updateCar game rInt) cars

updateCar :: GTA -> Int -> Car -> Car
updateCar game rInt car@Car{velocity} = case velocity of
  0 -> car
  1 | canMove 4 car walls'  -> newCarPosition $ changeDirR rInt car
    | canMove 1 car cars' || canMove 1 (player game) [car] -> car -- collision
    | canMove 4 car blocks' -> newCarPosition car
    | otherwise -> newCarPosition $ changeDir 0 car
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
  | canMove 1 person (cars game) = changeDir rInt person
  | canMove 1 person people' || canMove 1 (player game) [person] = changeDirR rInt person
  | canMove 4 person blocks' = newPersonPosition person { personSprite = Sprite { spriteType = spriteType (personSprite person),  spriteState = sprite' }}
  | otherwise = changeDirR rInt person
    where blocks' = moveBlocks (blocks game) [Sidewalk]
          people' = take personIndex (people game) ++ drop (1 + personIndex) (people game)
          personIndex = fromJust (elemIndex person (people game))
          sprite' | mod' (roundDecimals (elapsedTime game) 2) 0.5 == 0 = nextWalking (personSprite person)
                  | otherwise = spriteState (personSprite person)

newPersonPosition :: Person -> Person
newPersonPosition person@(Person _ _ d)
  | d == North = move (Position 0    1)  person
  | d == West  = move (Position (-1) 0)  person
  | d == South = move (Position 0  (-1)) person
  | otherwise  = move (Position 1    0)  person
