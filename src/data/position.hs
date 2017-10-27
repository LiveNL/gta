{-# LANGUAGE DeriveGeneric #-}
module Data.Position where

import GHC.Generics

data Position = Position
  { x :: Float,
    y :: Float }
  deriving (Show, Generic)

class Movable a where
  getPos :: a -> Position
  setPos :: Position -> a -> a
  getDir :: a -> Direction
  setDir :: Direction -> a -> a

move :: (Movable a) => Position -> a -> a
move (Position dx dy) a = setPos (Position (x + dx) (y + dy)) a
  where (Position x y) = getPos a

data Direction = North | West | South | East
  deriving (Show, Enum, Eq)

next :: Direction -> Direction
next East = North
next d = succ d

changeDir :: (Movable a) => a -> a
changeDir a = setDir (next x) a
  where x = getDir a
