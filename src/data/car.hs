module Data.Car where

import Graphics.Gloss
import Data.Position

data Car = Car
  { carPosition :: Position,
    carColor    :: Color }
  deriving Show

instance Movable Car where
  getPos (Car a _) = Position (x a) (y a)
  setPos (Position x' y') (Car _ c) = Car { carPosition = Position { x = x', y = y' },
                                            carColor = c }
