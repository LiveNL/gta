module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Debug.Trace

import Helpers
import Traffic
import Data.Block
import Data.Car
import Data.Game
import Data.Person
import Data.Position

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
initialState = loadWorld

updateKeyState :: (KeyState, KeyState, KeyState, KeyState) -> GTA -> GTA
updateKeyState (left', right', up', down') game = updateGame
  where updateGame = game { player = Player
          { playerPosition  = getPos (player game),
            keys            = Keys { left = left', right = right', up = up', down = down' },
            playerDirection = getDir (player game) }
        }

updatePlayerPosition :: GTA -> GTA
updatePlayerPosition game
  | canMove (x newPosition', y newPosition', currentDir) blocks' = updateGame
  | otherwise = game
  where
    currentKeys = keys (player game)
    currentDir  = getDir (player game)
    newPosition' = newPosition currentKeys (getPos (player game))
    updateGame = game { player = Player
      { playerPosition  = newPosition',
        keys            = currentKeys,
        playerDirection = currentDir }
    }
    blocks' = moveBlocks (blocks game) [Sidewalk, Road]

newPosition :: Keys -> Position -> Position
newPosition (Keys Down _    _    _   ) (Position x y) = Position {x = x - 1, y = y     }
newPosition (Keys _    Down _    _   ) (Position x y) = Position {x = x + 1, y = y     }
newPosition (Keys _    _    Down _   ) (Position x y) = Position {x = x    , y = y + 1 }
newPosition (Keys _    _    _    Down) (Position x y) = Position {x = x    , y = y - 1 }
newPosition (Keys _    _    _    _   ) (Position x y) = Position {x = x    , y = y     }

playerDraw :: GTA -> Picture
playerDraw game = translate x y $ color red $ rectangleSolid 10 10
  where Position x y = getPos (player game)

render :: GTA -> Picture
render game = pictures (blockList ++ carsList ++ personList ++ [playerDraw game])
  where blockList = map block (blocks game)
        carsList = map car (cars game)
        personList = map person (people game)

handleKeys :: Event -> GTA -> GTA
handleKeys (EventKey (SpecialKey KeyUp)    state _ _) = updateKeyState (Up   , Up   , state, Up   )
handleKeys (EventKey (SpecialKey KeyDown)  state _ _) = updateKeyState (Up   , Up   , Up   , state)
handleKeys (EventKey (SpecialKey KeyLeft)  state _ _) = updateKeyState (state, Up   , Up   , Up   )
handleKeys (EventKey (SpecialKey KeyRight) state _ _) = updateKeyState (Up   , state, Up   , Up   )
handleKeys _                                          = id

update :: Float -> GTA -> GTA
update _ = updateTraffic . updatePlayerPosition

updateTraffic :: GTA -> GTA
updateTraffic game = updatePeople (people game) (updateCars (cars game) game)
