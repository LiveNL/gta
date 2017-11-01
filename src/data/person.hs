{-# LANGUAGE DeriveGeneric, NamedFieldPuns #-}
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
  getPos Person{personPosition} = Position (x personPosition) (y personPosition)
  setPos (Position x' y') person@Person{personPosition} =
    person { personPosition = Position { x = x', y = y' } }

  getDir Person{personDirection} = personDirection
  setDir x person@Person{personDirection} = person { personDirection = x }

  width _ = 10
  height _ = 10

instance FromJSON Person
