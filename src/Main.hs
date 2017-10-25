module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Debug.Trace

-- Data types
data GTA = Game
  { player :: Player }
  deriving Show

data Position = Position
  { x :: Float,
    y :: Float,
    z :: Float }
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
update _ game = updatePlayerPosition game

canMove :: (Float, Float) -> Bool
canMove (x,y) = all canMove' blocks
  where canMove' block = not (inBlock (x,y) (coordinates block))

inBlock :: (Float, Float) -> Path -> Bool
inBlock (x,y) [(x1,y1), _, (x2,y2), _]
  | x >= x1 && x <= x2 && y >= y1 && y <= y2 = True
  | otherwise = False

coordinates :: Picture -> Path
coordinates (Translate x y color) = color' x y color
  where color' x y (Color _ polygon) = polygon' x y polygon
        polygon' x y (Polygon xs) = map (f x y) xs
        f x y z = (fst z + x, snd z + y)

block :: Picture
block = translate (-15) 0 $ color white $ rectangleSolid 10 100

car :: Picture
car = translate 0 30 $ color blue $ rectangleSolid 20 30

person :: Picture
person = translate 20 20 $ color green $ rectangleSolid 10 10

blocks :: [Picture]
blocks = [block, car, person]
