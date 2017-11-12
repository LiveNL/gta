{-# LANGUAGE DeriveGeneric, OverloadedStrings, NamedFieldPuns, DeriveAnyClass #-}
module Models.Game where

import Data.Aeson
import Data.Maybe
import Control.Monad
import GHC.Generics
import System.Directory
import System.IO
import Data.Fixed (mod')

import qualified Data.ByteString.Lazy.Char8 as B

import Models.Block
import Models.Car
import Models.Person
import Models.Player
import Models.Position

data GTA = Game
  { player :: Player,
    cars   :: [Car],
    people :: [Person],
    blocks :: [Block],
    gameState :: GameState,
    elapsedTime :: Float,
    highscore :: Int,
    timeLeft :: Float,
    coinCount :: (Int, Int) }
  deriving (Show, Generic)

data GTAJSON = GameJSON
  { blocksJSON    :: [Block],
    peopleJSON    :: [Person],
    carsJSON      :: [Car],
    highscoreJSON :: Int }
  deriving (Show, Generic, FromJSON, ToJSON)

data GameState = Loading | Running | Paused | Init | Dead | GameOver | Completed
  deriving (Show, Eq, Generic)

instance FromJSON GameState where
  parseJSON (String s) = maybe mzero return $ stringToGameState s
  parseJSON _ = mzero

jsonFile :: FilePath
jsonFile = "./config/world.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

readJSON :: IO GTAJSON
readJSON = do x <- (decode <$> getJSON) :: IO (Maybe GTAJSON)
              return ((fromJust x) :: GTAJSON)

readWorld :: IO GTA
readWorld = do x <- readJSON
               return Game { cars = carsJSON x, people = peopleJSON x, blocks = blocksJSON x, highscore = highscoreJSON x }

readHighscore :: Handle -> IO Int
readHighscore x = do ineof <- hIsEOF x
                     if ineof then return 0
                              else do r <- hGetLine x
                                      return (read r :: Int)

writeHighscore :: IO GTA -> IO GTA
writeHighscore game = do inh <- openFile "./config/highscore.txt" ReadWriteMode
                         t <- readHighscore inh
                         _ <- hSeek inh AbsoluteSeek 0 
                         hPutStr inh (show (t+1))
                         hClose inh
                         game

writeJSON :: IO GTA -> IO GTA
writeJSON game = do g <- game
                    if ((points) (player g) >= (highscore g) && mod' (roundDecimals (elapsedTime g) 2) 2.5 == 0)
                     then do r <- readJSON
                             B.writeFile "./config/world_new.json" (encode GameJSON { carsJSON = carsJSON r, peopleJSON = peopleJSON r, blocksJSON = blocksJSON r, highscoreJSON = highscore g })
                             return g
                     else return g

stringToGameState s
  | s == "Running" = Just Running
  | otherwise = Nothing

updatePoints :: GTA -> GTA
updatePoints game@Game{player} = game { player = updatePoints' player, highscore = updateHighscore player }
  where updatePoints' player@Player{points} = player { points = (points + 1)}
        updateHighscore player@Player{points} | points >= highscore game = points + 1
                                              | otherwise                = highscore game

