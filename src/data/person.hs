module Data.Person where

import Graphics.Gloss
import Data.Position

data Person = Person
  { personPosition :: Position,
    personColor    :: Color }
  deriving Show

instance Movable Person where
  getPos (Person a _) = Position (x a) (y a)
  setPos (Position x' y') (Person _ c) = Person { personPosition = Position { x = x', y = y' },
                                                  personColor = c }
