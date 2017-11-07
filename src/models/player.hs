{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NamedFieldPuns, OverloadedStrings #-}
module Models.Player where

import Control.Monad
import Data.Aeson
import Data.Fixed (mod')
import Data.List
import Data.Maybe
import Graphics.Gloss.Interface.IO.Game
import GHC.Generics

import Helpers
import Models.Block
import Models.Car
import Models.Person
import Models.Position


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

data PlayerState = Walking | Driving
  deriving (Show, Eq, Generic)

-- TODO: MOVE TO KEYS?
data Keys = Keys
  { left  :: KeyState,
    right :: KeyState,
    up    :: KeyState,
    down  :: KeyState }
  deriving (Show, Generic, FromJSON)

instance FromJSON KeyState where
  parseJSON (String s) = maybe mzero return $ stringToKeyState s
  parseJSON _ = mzero

--stringToKeyState :: Text -> Maybe KeyState
stringToKeyState s
  | s == "Up" = Just Up
  | s == "Down" = Just Down
  | otherwise = Nothing

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

updatePlayerPosition' :: Player -> Float -> Player
updatePlayerPosition' player@Player{playerSprite} elapsedTime =
  player { playerPosition = (fst newPosition'), playerVelocity = (snd newPosition'), playerSprite = (sprite player elapsedTime) }
    where newPosition' = newPosition (keys player) (getPos player)

sprite :: Player -> Float -> Sprite
sprite player@Player{playerSprite} elapsedTime = Sprite { spriteType = spriteType playerSprite, spriteState = spriteState' }
  where spriteState' = case playerState player of
                         Walking | mod' (roundDecimals elapsedTime 2) 0.25 == 0 -> nextSprite playerSprite
                                 | otherwise -> spriteState playerSprite
                         Driving -> spriteState playerSprite

newPosition :: Keys -> Position -> (Position, Float)
newPosition (Keys Down _    _    _   ) (Position x y) = (Position {x = x - 2, y = y     }, 1)
newPosition (Keys _    Down _    _   ) (Position x y) = (Position {x = x + 2, y = y     }, 1)
newPosition (Keys _    _    Down _   ) (Position x y) = (Position {x = x    , y = y + 2 }, 1)
newPosition (Keys _    _    _    Down) (Position x y) = (Position {x = x    , y = y - 2 }, 1)
newPosition (Keys _    _    _    _   ) (Position x y) = (Position {x = x    , y = y     }, 0)

carToPlayer :: Player -> Player
carToPlayer player = player {playerWidth = w', playerHeight = h', playerSprite = s', playerPosition = p', playerState = Walking}
  where
    w' = 10
    h' = 10
    s' = Sprite { spriteType = "player1", spriteState = 1 }
    p' = Position { x = (x' + 35), y = y'}
    Position x' y' = getPos player

playerToCar :: Player -> Car -> Player
playerToCar player car =
  player {playerWidth = w', playerHeight = h', playerSprite = s', playerState = Driving, playerDirection = d', playerPosition = p'}
    where
      w' = width car
      h' = height car
      s' = carSprite car
      d' = getDir car
      p' = getPos car

closeCars :: Player -> [Car] -> [Bool]
closeCars p c = map (canMove 1 p ) c'
  where c' = [[x] | x <- c]

killPlayer :: Player -> Player
killPlayer player@Player{} = player { playerSprite = Sprite { spriteType = "player1", spriteState =  2 }, playerVelocity = 2 }

