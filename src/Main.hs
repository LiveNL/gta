module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

-- Data types
data GTA = Game 
  {
    player :: Player
  }
  deriving Show

data Position = Position
  { 
    x :: Float,
    y :: Float,
    z :: Float
  }
  deriving Show

data Player = Player
  {
    position :: Position
  }
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
  {
    player = Player
      {
        position = Position { x = 0, y = 0, z = 0 }
      }
  }

updatePlayerPosition :: (Maybe Float, Maybe Float, Maybe Float) -> GTA -> GTA
updatePlayerPosition (x', y', z') game = game { 
                                                player = Player 
                                                  { 
                                                    position = Position { x = x''', y = y''', z = 0 }
                                                  }
                                              }
                                       where x''' | x' == Nothing = x''
                                                  | otherwise     = x'' + (convert x')
                                             y''' | y' == Nothing = y''
                                                  | otherwise     = y'' + (convert y')
                                             (x'', y'', _) = getPlayerPosition game

convert :: (Maybe a) -> a
convert (Just a) = a

getPlayerPosition :: GTA -> (Float, Float, Float)
getPlayerPosition game = (x', y', z')
                       where user         = player game
                             location     = position user
                             (x', y', z') = (x location, y location, z location)


playerD :: GTA -> Picture -- * Adjust name
playerD game = trace ( show (player game)) $ translate x y $ color red $ rectangleSolid 10 10
             where (x, y, z) = getPlayerPosition game

render :: GTA -> Picture
render = playerD

handleKeys :: Event -> GTA -> GTA
handleKeys (EventKey (SpecialKey KeyUp)    Down _ _) game = updatePlayerPosition (Nothing  , Just 1   , Nothing) game
handleKeys (EventKey (SpecialKey KeyDown)  Down _ _) game = updatePlayerPosition (Nothing  , Just (-1), Nothing) game
handleKeys (EventKey (SpecialKey KeyLeft)  Down _ _) game = updatePlayerPosition (Just (-1), Nothing  , Nothing) game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = updatePlayerPosition (Just 1   , Nothing  , Nothing) game
handleKeys _                                         game = game

update :: Float -> GTA -> GTA
update _ game = game
