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
initialState = Game
  { player = Player {
      position = Position { x = 0, y = 0, z = 0 },
      keys     = Keys { left = Up, right = Up, up = Up, down = Up }
    },
    cars = [Car
      { carPosition = Position { x = 0, y = -80, z = 0 }, carColor = blue }
    ],
    people = [Person
      { personPosition = Position { x = 20, y = 40, z = 0 }, personColor = yellow }
    ],
    world = [Block
      { blockPosition = Position { x = -20, y = 0, z = 0 }, blockWidth = 10, blockHeight = 100, blockType = Building }, Block
      { blockPosition = Position { x = 0, y = 100, z = 0 }, blockWidth = 230, blockHeight = 10, blockType = Building }, Block
      { blockPosition = Position { x = -80, y = 0, z = 0 }, blockWidth = 10, blockHeight = 200, blockType = Building }, Block
      { blockPosition = Position { x = -50, y = -100, z = 0 }, blockWidth = 100, blockHeight = 10, blockType = Building }, Block
      { blockPosition = Position { x = 120, y = 0, z = 0 }, blockWidth = 10, blockHeight = 200, blockType = Building }
    ]
  }

updateKeyState :: (KeyState, KeyState, KeyState, KeyState) -> GTA -> GTA
updateKeyState (left', right', up', down') game = updateGame
  where updateGame = game { player = Player
          { position = playerPosition game,
            keys     = Keys { left = left', right = right', up = up', down = down' } }
        }

updatePlayerPosition :: GTA -> GTA
updatePlayerPosition game
  | canMove (x newPosition', y newPosition', z newPosition') ((map person (people game)) ++ (map block (world game)) ++ (map car (cars game))) = updateGame
  | otherwise = game
  where
    currentKeys = keys (player game)
    newPosition' = newPosition currentKeys (playerPosition game)
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

playerPosition :: GTA -> Position
playerPosition game = position (player game)

playerDraw :: GTA -> Picture
playerDraw game = translate x y $ color red $ rectangleSolid 10 10
  where Position x y _ = playerPosition game

render :: GTA -> Picture
render game = pictures (blockList ++ carsList ++ personList ++ [playerDraw game])
  where blockList = map block (world game)
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
