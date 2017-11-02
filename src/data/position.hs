{-# LANGUAGE DeriveGeneric, NamedFieldPuns #-}
module Data.Position where

import GHC.Generics
import Data.Aeson

data Position = Position
  { x :: Float,
    y :: Float }
  deriving (Show, Generic, Eq)

instance FromJSON Position

class Movable a where
  getPos :: a -> Position
  setPos :: Position -> a -> a
  getDir :: a -> Direction
  setDir :: Direction -> a -> a
  width  :: a -> Float
  height :: a -> Float

move :: (Movable a) => Position -> a -> a
move (Position dx dy) a = setPos (Position (x + dx) (y + dy)) a
  where (Position x y) = getPos a

data Direction = North | West | South | East
  deriving (Enum, Eq, Generic)

instance Show Direction where
  show North = "n"
  show West = "w"
  show East = "e"
  show South = "s"

instance FromJSON Direction

coordinates :: (Movable a) => a -> [(Float, Float)]
coordinates a = [(x'-w',y'-h'),(x'+w',y'-h'),(x'+w',y'+h'),(x'-w',y'+h')]
  where (Position x' y') = getPos a
        w' = (width a) / 2
        h' = (height a) / 2

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

data Sprite = Sprite
  {
    spriteType  :: SpriteType,
    spriteState :: Int
  }
  deriving (Show, Eq, Generic)

data SpriteType = Person1 | Person2 | Person3 | Car1 | Car2 | Car3
  deriving (Show, Eq, Generic)

instance FromJSON Sprite
instance FromJSON SpriteType

{-instance Show SpriteType where
  show Person1_1 = "p1_1"
  show Person1_2 = "p1_2"
  show Person1_3 = "p1_3"

  show Person2_1 = "p2_1"
  show Person2_2 = "p2_2"
  show Person2_3 = "p2_3"

  show Car1 = "c1"
  show Car2 = "c2"
  show Car3 = "c3"-}

nextWalking :: Sprite -> Int
nextWalking (Sprite t s) | t == Car1 || t == Car2 || t == Car3 = 1
                         | s == 3 = 1
                         | otherwise = succ s











