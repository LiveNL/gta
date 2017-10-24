module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Data types
data GTA = Game {}
  deriving Show

-- Variables
width, height, offset :: Int
width = 300
height = 300
offset = 100

-- Functions
main :: IO ()
main = play window black 60 initialState render handleKeys update

window :: Display
window = InWindow "GTA" (width, height) (offset, offset)

initialState :: GTA
initialState = Game {}

render :: GTA -> Picture
render game = translate 1 1 $ rectangleSolid 10 10

handleKeys :: Event -> GTA -> GTA
handleKeys _ game = game

update :: Float -> GTA -> GTA
update _ game = game
