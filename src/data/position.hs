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

data Direction = North | East | South | West
  deriving (Show, Enum, Eq, Generic)

instance FromJSON Direction

coordinates :: (Movable a) => a -> [(Float, Float)]
coordinates a = [(x'-w',y'-h'),(x'+w',y'-h'),(x'+w',y'+h'),(x'-w',y'+h')]
  where (Position x' y') = getPos a
        w' = (width a) / 2
        h' = (height a) / 2

next :: Direction -> Direction
next West = North
next d = succ d

prev :: Direction -> Direction
prev North = West
prev d = pred d

changeDir :: (Movable a) => Int -> a -> a
changeDir rInt a = case rInt of
                     0 -> setDir (next x) a
                     1 -> setDir x a
  where x = getDir a

roundDecimals :: (Fractional a2, RealFrac a1, Integral b) => a1 -> b -> a2
roundDecimals f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

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

nextWalking :: Sprite -> Int
nextWalking (Sprite t s) | t == Car1 || t == Car2 || t == Car3 = 1
                         | s == 3 = 1
                         | otherwise = succ s

changeDirR :: (Movable a) => Int -> a -> a
changeDirR rInt a = case rInt of
                      0 -> setDir (next x) a
                      1 -> setDir (prev x) a
  where x = getDir a
