module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Debug.Trace

import Traffic
import Data.Position

-- Data types
data GTA = Game
  { player :: Player }
  deriving Show

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

-- Variables
width, height, offset :: Int
width  = 300
height = 300
offset = 100

-- Functions
main :: IO ()
main = play window black 60 initialState render handleKeys update

window :: Display
window = InWindow "GTA" (width, height) (offset, offset)

initialState :: GTA
initialState = Game
  { player = Player
      { position = Position { x = 0, y = 0, z = 0 },
        keys     = Keys { left = Up, right = Up, up = Up, down = Up }
      }
  }

updateKeyState :: (KeyState, KeyState, KeyState, KeyState) -> GTA -> GTA
updateKeyState (left', right', up', down') game = updateGame
  where cPosition = position (player game)
        updateGame = game { player = Player
          { position = cPosition,
            keys     = Keys { left = left', right = right', up = up', down = down' } }
        }

updatePlayerPosition :: GTA -> GTA
updatePlayerPosition game
  | canMove (x newPosition', y newPosition') = updateGame
  | otherwise = game
  where
    currentKeys = keys (player game)
    currentPosition = position (player game)
    newPosition' = newPosition currentKeys currentPosition
    updateGame = game { player = Player
      { position = newPosition',
        keys     = currentKeys }
    }

newPosition :: Keys -> Position -> Position
newPosition (Keys Down _    _    _   ) (Position x y _) = Position {x = x - 1, y = y    , z = 0 }
newPosition (Keys _    Down _    _   ) (Position x y _) = Position {x = x + 1, y = y    , z = 0 }
newPosition (Keys _    _    Down _   ) (Position x y _) = Position {x = x    , y = y + 1, z = 0 }
newPosition (Keys _    _    _    Down) (Position x y _) = Position {x = x    , y = y - 1, z = 0 }
newPosition (Keys _    _    _    _   ) (Position x y _) = Position {x = x    , y = y    , z = 0 }

playerPosition :: GTA -> (Float, Float, Float)
playerPosition game = (x', y', z')
                       where user         = player game
                             location     = position user
                             (x', y', z') = (x location, y location, z location)

playerDraw :: GTA -> Picture
playerDraw game = translate x y $ color red $ rectangleSolid 10 10
                where (x, y, z) = playerPosition game

render :: GTA -> Picture
render game = pictures (blocks ++ [playerDraw game])

handleKeys :: Event -> GTA -> GTA
handleKeys (EventKey (SpecialKey KeyUp)    state _ _) = updateKeyState (Up   , Up   , state, Up   )
handleKeys (EventKey (SpecialKey KeyDown)  state _ _) = updateKeyState (Up   , Up   , Up   , state)
handleKeys (EventKey (SpecialKey KeyLeft)  state _ _) = updateKeyState (state, Up   , Up   , Up   )
handleKeys (EventKey (SpecialKey KeyRight) state _ _) = updateKeyState (Up   , state, Up   , Up   )
handleKeys _                                          = id

update :: Float -> GTA -> GTA
update _ = updatePlayerPosition

updateTraffic :: GTA -> GTA
updateTraffic game = updateCars (cars game) game
