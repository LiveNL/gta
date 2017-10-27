{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Car where

import Data.Aeson
import Graphics.Gloss
import Data.Position
import Data.Color
import GHC.Generics

data Car = Car
  { carPosition  :: Position,
    carColor     :: Color,
    carDirection :: Direction }
  deriving (Show, Generic)

instance Movable Car where
  getPos (Car a _ d) = Position (x a) (y a)
  setPos (Position x' y') (Car _ c d) = Car { carPosition = Position { x = x', y = y' },
                                              carColor = c,
                                              carDirection = d }
  getDir (Car _ _ d) = d
  setDir x (Car a c _) = Car { carPosition = a, carColor = c, carDirection = x }

instance FromJSON Car
