{-# LANGUAGE DeriveGeneric #-}
module Data.Person where

import Data.Aeson
import Graphics.Gloss
import Data.Position
import GHC.Generics
import Data.Color

data Person = Person
  { personPosition  :: Position,
    personColor     :: Color,
    personDirection :: Direction }
  deriving (Show, Generic)

instance Movable Person where
  getPos (Person a _ d) = Position (x a) (y a)
  setPos (Position x' y') (Person _ c d) = Person { personPosition = Position { x = x', y = y' },
                                                    personColor = c,
                                                    personDirection = d }
  getDir (Person _ _ d) = d
  setDir x (Person a c _) = Person { personPosition = a, personColor = c, personDirection = x }

instance FromJSON Person
