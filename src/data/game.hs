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
    playerDirection :: Direction,
    playerHeight    :: Float,
    playerWidth     :: Float,
    playerSprite    :: Sprite,
    playerVelocity  :: Float,
    playerState     :: PlayerState,
    points          :: Int }
  deriving (Show)

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
    gameState :: GameState,
    elapsedTime :: Float }
  deriving (Show, Generic)

data GTAJSON = GameJSON
  { carsJSON   :: [Car],
    peopleJSON :: [Person],
    blocksJSON :: [Block] }
  deriving (Show, Generic)

data GameState = Loading | Running | Paused | Dead
  deriving (Show, Eq, Generic)

data PlayerState = Walking | Driving
  deriving (Show, Eq, Generic)

jsonFile :: FilePath
jsonFile = "./config/world.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

readJSON :: IO GTAJSON
readJSON = do x <- (decode <$> getJSON) :: IO (Maybe GTAJSON)
              return ((fromJust x) :: GTAJSON)

readWorld :: IO GTA
readWorld = do x <- readJSON
               return Game { cars = carsJSON x, people = peopleJSON x, blocks = blocksJSON x } -- Missing fields are added in Main.hs

instance FromJSON GTAJSON
instance FromJSON Keys
instance FromJSON PlayerState

instance Movable Player where
  getPos Player{playerPosition} = Position (x playerPosition) (y playerPosition)
  setPos (Position x' y') player@Player{playerPosition} =
    player { playerPosition = Position { x = x', y = y' } }

  getDir Player{playerDirection} = playerDirection
  setDir x player@Player{playerDirection} = player { playerDirection = x }

  width p = if (playerDirection p) == North || (playerDirection p) == South
            then playerWidth p
            else playerHeight p

  height p = if (playerDirection p) == East || (playerDirection p) == West
             then playerWidth p
             else playerHeight p

  getSprite player@Player{playerSprite} = Sprite (spriteType playerSprite) state
    where state = case playerVelocity player of
                     0 -> 1
                     1 -> spriteState playerSprite
                     2 -> 0

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
