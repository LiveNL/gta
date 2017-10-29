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

  coordinates car@Car{carPosition} = [(x' - w',y' - h'),(x' + w', y'+ h')]
    where (Position x' y') = getPos car
          w' = 20 / 2
          h' = 30 / 2

  width _ = 20
  height _ = 30

instance FromJSON Car
