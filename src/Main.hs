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
  { position :: Position }
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
      { position = Position { x = 0, y = 0, z = 0 } }
  }

updatePlayerPosition :: (Maybe Float, Maybe Float, Maybe Float) -> GTA -> GTA
updatePlayerPosition (x', y', z') game = game { player = Player
                                                  { position = Position { x = newX, y = newY, z = 0 } }
                                              }
                                       where (x'', y'', _) = playerPosition game
                                             newX = newPosition x' x''
                                             newY = newPosition y' y''

newPosition :: Maybe Float -> Float -> Float
newPosition Nothing    old = old
newPosition (Just new) old = old + new

playerPosition :: GTA -> (Float, Float, Float)
playerPosition game = (x', y', z')
                       where user         = player game
                             location     = position user
                             (x', y', z') = (x location, y location, z location)

playerDraw :: GTA -> Picture
playerDraw game = translate x y $ color red $ rectangleSolid 10 10
                where (x, y, z) = playerPosition game

render :: GTA -> Picture
render game = pictures [ playerDraw game,
                         blocks ]

handleKeys :: Event -> GTA -> GTA
handleKeys (EventKey (SpecialKey KeyUp)    Down _ _) game = updatePlayerPosition (Nothing  , Just 1   , Nothing) game
handleKeys (EventKey (SpecialKey KeyDown)  Down _ _) game = updatePlayerPosition (Nothing  , Just (-1), Nothing) game
handleKeys (EventKey (SpecialKey KeyLeft)  Down _ _) game = updatePlayerPosition (Just (-1), Nothing  , Nothing) game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = updatePlayerPosition (Just 1   , Nothing  , Nothing) game
handleKeys _                                         game = game

update :: Float -> GTA -> GTA
update _ game = game

canMove :: (Float, Float) -> Bool
canMove (x,y) = not (inBlock (x,y) (coordinates block))

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

blocks :: Picture
blocks = pictures [block]
