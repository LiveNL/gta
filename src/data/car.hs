module Data.Car where

import Graphics.Gloss
import Data.Position

data Car = Car
  { carPosition  :: Position,
    carColor     :: Color,
    carDirection :: Direction }
  deriving Show

instance Movable Car where
  getPos (Car a _ d) = Position (x a) (y a)
  setPos (Position x' y') (Car _ c d) = Car { carPosition = Position { x = x', y = y' },
                                              carColor = c,
                                              carDirection = d }
  getDir (Car _ _ d) = d
  setDir x (Car a c _) = Car { carPosition = a, carColor = c, carDirection = x }
