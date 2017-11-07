{-# LANGUAGE NamedFieldPuns #-}
module Traffic (updatePeople) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Data.Maybe
import Data.Fixed (mod')

import Helpers

import Models.Block
import Data.Position
import Data.Person
import Debug.Trace

import Models.Car
import Models.Game
import Models.Player

updatePeople :: [Person] -> Int -> GTA -> GTA
updatePeople people rInt game = game { people = updatePeople' }
  where updatePeople' = map (updatePerson game rInt) people

updatePerson :: GTA -> Int -> Person -> Person
updatePerson game rInt person
  | personVelocity person == 0 = person
  | canMove 1 person (cars game) = changeDir rInt person
  | canMove 1 person people' = changeDirR rInt person
  | canMove 1 person [player game] = changeDirR rInt person
  | canMove 4 person blocks' = newPersonPosition person { personSprite = Sprite { spriteType = spriteType (personSprite person), spriteState = sprite' }}
  | otherwise = changeDirR rInt person
    where blocks' = moveBlocks (blocks game) [Sidewalk]
          people' = take personIndex (people game) ++ drop (1 + personIndex) (people game)
          personIndex = fromJust (elemIndex person (people game))
          sprite' | mod' (roundDecimals (elapsedTime game) 2) 0.5 == 0 = nextSprite (personSprite person)
                  | otherwise = spriteState (personSprite person)

newPersonPosition :: Person -> Person
newPersonPosition person@(Person _ _ d _)
  | d == North = move (Position 0    1)  person
  | d == West  = move (Position (-1) 0)  person
  | d == South = move (Position 0  (-1)) person
  | otherwise  = move (Position 1    0)  person
