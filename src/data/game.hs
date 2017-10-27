{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
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
    loaded :: Float }
  deriving (Show, Generic)

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
  getPos (Player a _ _) = Position (x a) (y a)
  setPos (Position x' y') (Player _ k _) = Player { playerPosition = Position { x = x', y = y' },
                                                  keys = k }
  getDir (Player _ _ d) = d
  setDir x (Player a k _) = Player { playerPosition = a, keys = k, playerDirection = x }

instance FromJSON KeyState where
  parseJSON (String s) = maybe mzero return $ stringToKeyState s
  parseJSON _ = mzero

--stringToKeyState :: Text -> Maybe KeyState
stringToKeyState s
  | s == "Up" = Just Up
  | s == "Down" = Just Down
  | otherwise = Nothing
