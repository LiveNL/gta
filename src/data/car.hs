module Data.Car where

import Graphics.Gloss
import Data.Position

data Car = Car
  { carPosition :: Position,
    carColor    :: Color }
  deriving Show
