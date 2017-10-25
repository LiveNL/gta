module Data.Person where

import Graphics.Gloss
import Data.Position

data Person = Person
  { personPosition :: Position,
    personColor    :: Color }
  deriving Show
