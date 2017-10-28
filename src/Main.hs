module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

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
main = playIO window black 60 initialState render handleKeys update

window :: Display
window = InWindow "GTA" (width, height) (offset, offset)

initialState :: GTA
initialState = Game
  { player = Player {
      playerPosition  = Position { x = 50, y = 0 },
      keys            = Keys { left = Up, right = Up, up = Up, down = Up },
      playerDirection = North
    },
    cars = [], people = [], blocks = [], gameState = Loading
  }

updateKeyState :: (KeyState, KeyState, KeyState, KeyState) -> GTA -> IO GTA
updateKeyState (left', right', up', down') game = return updateGame
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

render :: GTA -> IO Picture
render game = return (pictures (blockList ++ carsList ++ personList ++ [playerDraw game]))
  where blockList = map block (blocks game)
        carsList = map car (cars game)
        personList = map person (people game)

handleKeys :: Event -> GTA -> IO GTA
handleKeys (EventKey (SpecialKey KeyUp)    state _ _) = updateKeyState (Up   , Up   , state, Up   )
handleKeys (EventKey (SpecialKey KeyDown)  state _ _) = updateKeyState (Up   , Up   , Up   , state)
handleKeys (EventKey (SpecialKey KeyLeft)  state _ _) = updateKeyState (state, Up   , Up   , Up   )
handleKeys (EventKey (SpecialKey KeyRight) state _ _) = updateKeyState (Up   , state, Up   , Up   )
handleKeys (EventKey (Char 'p')            Down  _ _) = pauseGame
handleKeys _                                          = return

pauseGame :: GTA -> IO GTA
pauseGame game
  | state == Running = return game { gameState = Paused }
  | otherwise        = return game { gameState = Running }
    where state = gameState game

update :: Float -> GTA -> IO GTA
update _ game
  | gameState game == Loading = readWorld
  | gameState game == Paused = return game
  | otherwise = return ( updateTraffic ( updatePlayerPosition game ) )

updateTraffic :: GTA -> GTA
updateTraffic game = updatePeople (people game) (updateCars (cars game) game)
