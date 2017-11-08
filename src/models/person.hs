{-# LANGUAGE DeriveGeneric, NamedFieldPuns, DeriveAnyClass #-}
module Models.Person where

import Data.Aeson
import Graphics.Gloss
import Data.Position
import GHC.Generics

data Person = Person
  { personPosition  :: Position,
    personSprite    :: Sprite,
    personDirection :: Direction,
    personVelocity  :: Int }
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

newPersonPosition :: Person -> Person
newPersonPosition person@(Person _ _ d _)
  | d == North = move (Position 0    1)  person
  | d == West  = move (Position (-1) 0)  person
  | d == South = move (Position 0  (-1)) person
  | otherwise  = move (Position 1    0)  person
