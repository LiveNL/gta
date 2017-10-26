module Data.Game where

import Graphics.Gloss.Interface.Pure.Game

import Data.Position
import Data.Car
import Data.Person
import Data.Block

data Player = Player
  { position :: Position,
    keys     :: Keys }
  deriving Show

data Keys = Keys
  { left  :: KeyState,
    right :: KeyState,
    up    :: KeyState,
    down  :: KeyState }
  deriving Show

data GTA = Game
  { player :: Player,
    cars   :: [Car],
    people :: [Person],
    world :: [Block] }
  deriving Show
