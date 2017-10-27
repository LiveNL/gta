{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Game where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

import Graphics.Gloss.Interface.Pure.Game

import Data.Position
import Data.Car
import Data.Person
import Data.Block

data Player = Player
  { playerPosition  :: Position,
    keys            :: Keys,
    playerDirection :: Direction}
  deriving (Show, Generic)

data Keys = Keys
  { left  :: KeyState,
    right :: KeyState,
    up    :: KeyState,
    down  :: KeyState }
  deriving (Show, Generic)

data GTA = Game
  { player :: Player,
    cars   :: [Car],
    people :: [Person],
    blocks :: [Block] }
  deriving (Show, Generic)

loadWorld :: GTA
--loadWorld = (decode <$> getJSON) :: IO (Maybe GTA)
--loadWorld = (decode <$> getJSON) :: IO (Maybe GTA)
loadWorld = Game
  { player = Player {
      playerPosition  = Position { x = 50, y = 0 },
      keys            = Keys { left = Up, right = Up, up = Up, down = Up },
      playerDirection = North
    },
    cars = [
      Car { carPosition = Position { x = 30, y = 30 },  carColor = blue,  carDirection = North },
      Car { carPosition = Position { x = 30, y = -80 }, carColor = green, carDirection = North }
    ],
    people = [Person
      { personPosition = Position { x = 20, y = 60 }, personColor = yellow, personDirection = North }
    ],
    blocks = [Block
      { blockPosition = Position { x = 0, y = 0 }, blockWidth = 200, blockHeight = 200, blockType = Road}, Block
      { blockPosition = Position { x = 0, y = 0 }, blockWidth = 10, blockHeight = 100, blockType = Building }, Block
      { blockPosition = Position { x = 0, y = 100 }, blockWidth = 200, blockHeight = 10, blockType = Building }, Block
      { blockPosition = Position { x = -100, y = 0 }, blockWidth = 10, blockHeight = 200, blockType = Building }, Block
      { blockPosition = Position { x = -50, y = -100 }, blockWidth = 150, blockHeight = 10, blockType = Building }, Block
      { blockPosition = Position { x = 100, y = 0 }, blockWidth = 10, blockHeight = 200, blockType = Building }
    ]
  }

jsonFile :: FilePath
jsonFile = "./config/world.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

--instance FromJSON GTA

instance Movable Player where
  getPos (Player a _ _) = Position (x a) (y a)
  setPos (Position x' y') (Player _ k _) = Player { playerPosition = Position { x = x', y = y' },
                                                  keys = k }
  getDir (Player _ _ d) = d
  setDir x (Player a k _) = Player { playerPosition = a, keys = k, playerDirection = x }
