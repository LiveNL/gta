{-# LANGUAGE DeriveGeneric, NamedFieldPuns #-}
module Data.Position where

import GHC.Generics
import Data.Aeson

data Position = Position
  { x :: Float,
    y :: Float }
  deriving (Show, Generic)

instance FromJSON Position

class Movable a where
  getPos :: a -> Position
  setPos :: Position -> a -> a
  getDir :: a -> Direction
  setDir :: Direction -> a -> a
  width  :: a -> Float
  height :: a -> Float
  coordinates :: a -> [(Float,Float)]

move :: (Movable a) => Position -> a -> a
move (Position dx dy) a = setPos (Position (x + dx) (y + dy)) a
  where (Position x y) = getPos a

data Direction = North | West | South | East
  deriving (Show, Enum, Eq, Generic)

instance FromJSON Direction

next :: Direction -> Direction
next East = North
next d = succ d

prev :: Direction -> Direction
prev North = East
prev d = pred d

changeDir :: (Movable a) => a -> Int -> a
changeDir a rInt = case rInt of
                     0 -> setDir (next x) a
                     1 -> setDir (prev x) a
  where x = getDir a
