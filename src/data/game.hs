{-# LANGUAGE DeriveGeneric, OverloadedStrings, NamedFieldPuns, DeriveAnyClass #-}
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
import Models.Player
import Data.Block
import Data.Maybe

import System.Directory

data GTA = Game
  { player :: Player,
    cars   :: [Car],
    people :: [Person],
    blocks :: [Block],
    gameState :: GameState,
    elapsedTime :: Float,
    highscore :: Int }
  deriving (Show, Generic)

data GTAJSON = GameJSON
  { blocksJSON    :: [Block],
    peopleJSON    :: [Person],
    carsJSON      :: [Car],
    highscoreJSON :: Int }
  deriving (Show, Generic, FromJSON, ToJSON)

data GameState = Loading | Running | Paused | Dead
  deriving (Show, Eq, Generic)

jsonFile :: FilePath
jsonFile = "./config/world.json"

updateFile :: IO ()
updateFile = do exists <- doesFileExist "./config/world_new.json"
                when exists (renameFile "./config/world_new.json" jsonFile)

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

readJSON :: IO GTAJSON
readJSON = do x <- (decode <$> getJSON) :: IO (Maybe GTAJSON)
              return ((fromJust x) :: GTAJSON)

readWorld :: IO GTA
readWorld = do _ <- updateFile
               x <- readJSON
               return (Game { cars = carsJSON x, people = peopleJSON x, blocks = blocksJSON x, highscore = highscoreJSON x })

writeJSON :: IO GTA -> IO GTA
writeJSON game = do g <- game
                    r <- readJSON
                    B.writeFile "./config/world_new.json" (encode GameJSON { carsJSON = carsJSON r, peopleJSON = peopleJSON r, blocksJSON = blocksJSON r, highscoreJSON = highscore g })
                    return g

instance FromJSON GameState where
  parseJSON (String s) = maybe mzero return $ stringToGameState s
  parseJSON _ = mzero

--stringToKeyState :: Text -> Maybe KeyState
stringToGameState s
  | s == "Running" = Just Running
  | otherwise = Nothing
