{-# LANGUAGE DeriveGeneric, OverloadedStrings, NamedFieldPuns #-}
module Data.Game where

import Data.Aeson
import Control.Monad
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Data.Position
import Data.Car
import Data.Person
import Data.Block
import Data.Maybe

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
    blocks :: [Block],
    gameState :: GameState }
  deriving (Show, Generic)

data GameState = Loading | Running | Paused
  deriving (Show, Eq, Generic)

readWorld :: IO GTA
readWorld = do x <- (decode <$> getJSON) :: IO (Maybe GTA)
               return ((fromJust x) :: GTA)

jsonFile :: FilePath
jsonFile = "./config/world.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

instance FromJSON GTA
instance FromJSON Player
instance FromJSON Keys

instance Movable Player where
  getPos Player{playerPosition} = Position (x playerPosition) (y playerPosition)
  setPos (Position x' y') player@Player{playerPosition} =
    player { playerPosition = Position { x = x', y = y' } }

  getDir Player{playerDirection} = playerDirection
  setDir x player@Player{playerDirection} = player { playerDirection = x }

  coordinates Player{playerPosition} = [(x' - w',y' - h'),(x' + w', y'+ h')]
    where x' = x playerPosition
          y' = y playerPosition
          w' = 10 / 2
          h' = 10 / 2

  width _ = 10
  height _ = 10

instance FromJSON KeyState where
  parseJSON (String s) = maybe mzero return $ stringToKeyState s
  parseJSON _ = mzero

--stringToKeyState :: Text -> Maybe KeyState
stringToKeyState s
  | s == "Up" = Just Up
  | s == "Down" = Just Down
  | otherwise = Nothing

instance FromJSON GameState where
  parseJSON (String s) = maybe mzero return $ stringToGameState s
  parseJSON _ = mzero

--stringToKeyState :: Text -> Maybe KeyState
stringToGameState s
  | s == "Running" = Just Running
  | otherwise = Nothing
