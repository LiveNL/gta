{-# LANGUAGE DeriveGeneric, NamedFieldPuns #-}
module Data.Car where

import Data.Aeson
import Graphics.Gloss
import GHC.Generics

import Data.Position
import Data.Color

data Car = Car
  { carPosition  :: Position,
    carColor     :: Color,
    carDirection :: Direction,
    velocity     :: Int}
  deriving (Show, Generic)

instance Movable Car where
  getPos Car{carPosition} = Position (x carPosition) (y carPosition)
  setPos (Position x' y') car@Car{carPosition} =
    car { carPosition = Position { x = x', y = y' } }

  getDir Car{carDirection} = carDirection
  setDir x car@Car{carDirection} = car { carDirection = x }

  width c = if (carDirection c) == North || (carDirection c) == South
              then 20
              else 30

  height c = if (carDirection c) == East || (carDirection c) == West
               then 20
               else 30

instance FromJSON Car
