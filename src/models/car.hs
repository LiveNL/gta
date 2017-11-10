{-# LANGUAGE DeriveGeneric, NamedFieldPuns, DeriveAnyClass #-}
module Models.Car where

import Data.Aeson
import Graphics.Gloss
import GHC.Generics

import Models.Color
import Models.Position

data Car = Car
  { carPosition  :: Position,
    carSprite    :: Sprite,
    carDirection :: Direction,
    velocity     :: Int }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)

instance Movable Car where
  getPos Car{carPosition} = Position (x carPosition) (y carPosition)
  setPos (Position x' y') car@Car{carPosition} =
    car { carPosition = Position { x = x', y = y' } }

  getDir Car{carDirection} = carDirection
  setDir x car@Car{carDirection} = car { carDirection = x }

  width c = if (carDirection c) == East || (carDirection c) == West
              then 30
              else 20

  height c = if (carDirection c) == East || (carDirection c) == West
               then 20
               else 30

  getSprite Car{carSprite} = Sprite (spriteType carSprite) (spriteState carSprite)

newCarPosition :: Car -> Car
newCarPosition car@(Car (Position _ _) _ d _) = case d of
  North -> move (Position 0    1)  car
  West  -> move (Position (-1) 0)  car
  South -> move (Position 0  (-1)) car
  _     -> move (Position 1    0)  car
