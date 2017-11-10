{-# LANGUAGE DeriveGeneric, NamedFieldPuns, DeriveAnyClass #-}
module Models.Position where

import Data.Aeson
import Data.Maybe
import Debug.Trace
import GHC.Generics
import Graphics.Gloss

data Position = Position
  { x :: Float,
    y :: Float }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)

class Movable a where
  getPos :: a -> Position
  setPos :: Position -> a -> a
  getDir :: a -> Direction
  setDir :: Direction -> a -> a
  width  :: a -> Float
  height :: a -> Float
  getSprite :: a -> Sprite

draw :: (Movable a) => [(String,Picture)] -> a -> Picture
draw images a = translate x y $ scale w'' h'' $ rotate angle $ image
  where angle = case d of
                   North -> 0
                   West  -> 270
                   South -> 180
                   East  -> 90

        w'' = if d == North || d == South
              then (width a) / fromIntegral w'
              else (width a) / fromIntegral h'

        h'' = if d == North || d == South
              then (height a) / fromIntegral h'
              else (height a) / fromIntegral w'

        image@(Bitmap w' h' _ True) = fromJust (lookup name images)
        name         = "./sprites/" ++ (spriteType s) ++ "_" ++ show (spriteState s) ++ ".bmp"
        d            = getDir a
        s            = getSprite a
        Position x y = getPos a

move :: (Movable a) => Position -> a -> a
move (Position dx dy) a = setPos (Position (x + dx) (y + dy)) a
  where Position x y = getPos a

data Direction = North | East | South | West
  deriving (Show, Enum, Eq, Generic, FromJSON, ToJSON)

coordinates :: (Movable a) => a -> [(Float, Float)]
coordinates a = [(x'-w',y'-h'),(x'+w',y'-h'),(x'+w',y'+h'),(x'-w',y'+h')]
  where (Position x' y') = getPos a
        w' = (width a) / 2
        h' = (height a) / 2

next :: Direction -> Direction
next West = North
next d    = succ d

prev :: Direction -> Direction
prev North = West
prev d     = pred d

changeDir :: (Movable a) => Int -> a -> a
changeDir rInt a = case rInt of
                     0 -> setDir (next x) a
                     1 -> setDir x a
                     2 -> setDir (next (next x)) a
  where x = getDir a

roundDecimals :: (Fractional a2, RealFrac a1, Integral b) => a1 -> b -> a2
roundDecimals f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

data Sprite = Sprite
  {
    spriteType  :: String,
    spriteState :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

nextSprite :: Sprite -> Int
nextSprite (Sprite t s) | s == 3 = 1
                        | otherwise = succ s

changeDirR :: (Movable a) => Int -> a -> a
changeDirR rInt a = case rInt of
                      0 -> setDir (next x) a
                      1 -> setDir x a
                      2 -> setDir (prev x) a
  where x = getDir a
