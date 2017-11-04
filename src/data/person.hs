{-# LANGUAGE DeriveGeneric, NamedFieldPuns, DeriveAnyClass #-}
module Data.Person where

import Data.Aeson
import Graphics.Gloss
import Data.Position
import GHC.Generics
import Data.Color

data Person = Person
  { personPosition  :: Position,
    personSprite     :: Sprite,
    personDirection :: Direction }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)

instance Movable Person where
  getPos Person{personPosition} = Position (x personPosition) (y personPosition)
  setPos (Position x' y') person@Person{personPosition} =
    person { personPosition = Position { x = x', y = y' } }

  getDir Person{personDirection} = personDirection
  setDir x person@Person{personDirection} = person { personDirection = x }

  width _ = 10
  height _ = 10

  getSprite Person{personSprite} = Sprite (spriteType personSprite) (spriteState personSprite)