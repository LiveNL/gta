module Data.Position where

data Position = Position
  { x :: Float,
    y :: Float }
  deriving Show

class Movable a where
  getPos :: a -> Position
  setPos :: Position -> a -> a

move :: (Movable a) => Position -> a -> a
move (Position dx dy) a = setPos (Position (x + dx) (y + dy)) a
  where (Position x y) = getPos a
