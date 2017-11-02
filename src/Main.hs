{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List

import Helpers
import Traffic
import Data.Block
import Data.Car
import Data.Game
import Data.Person
import Data.Position
import Data.Fixed (mod')
import Data.Monoid
import Data.Maybe
import Debug.Trace
import System.Random

-- Variables
windowWidth, windowHeight, offset :: Int
windowWidth  = 600
windowHeight = 600
offset = 100

-- Functions
main :: IO ()
main = playIO window black 60 initialState render handleKeys update

window :: Display
window = InWindow "GTA" (windowWidth, windowHeight) (offset, offset)

initialState :: GTA
initialState = Game
  { player = Player {
      playerPosition  = Position { x = 50, y = 0 },
      keys            = Keys { left = Up, right = Up, up = Up, down = Up },
      playerDirection = North, playerWidth = 10, playerHeight = 10, playerColor = red,
      playerSprite    = Walking1
    },
    cars = [], people = [], blocks = [], gameState = Loading, elapsedTime = 0
  }

updateKeyState :: (KeyState, KeyState, KeyState, KeyState, Direction) -> GTA -> IO GTA
updateKeyState (left', right', up', down', direction') game = return updateGame
  where updateGame = game { player = Player
          { playerPosition  = getPos (player game),
            keys            = Keys { left = left', right = right', up = up', down = down' },
            playerDirection = direction',
            playerWidth     = playerWidth (player game),
            playerHeight    = playerHeight (player game),
            playerColor     = playerColor (player game),
            playerSprite    = playerSprite (player game) }
        }

roundDecimals :: (Fractional a2, RealFrac a1, Integral b) => a1 -> b -> a2
roundDecimals f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

updatePlayerPosition :: GTA -> GTA
-- TODO: FIX THE DUPLICATION FOR CAN MOVE, but works for now
updatePlayerPosition game
  | canMove (player game) (cars game) && canMove (player game) (people game) &&
      canMove (player game) blocks'
    = updateGame
  | otherwise = game
  where
    currentKeys = keys (player game)
    currentDir  = getDir (player game)
    newPosition' = newPosition currentKeys (getPos (player game))
    updateGame = game { player = Player
      { playerPosition  = newPosition',
        keys            = currentKeys,
        playerDirection = currentDir,
        playerWidth     = playerWidth (player game),
        playerHeight    = playerHeight (player game),
        playerColor     = playerColor (player game),
        playerSprite    = sprite }
    }
    blocks' = moveBlocks (blocks game) [Sidewalk, Road]
    sprite | mod' (roundDecimals (elapsedTime game) 2) 0.5 == 0 = nextWalking (playerSprite (player game))
           | otherwise = playerSprite (player game)

newPosition :: Keys -> Position -> Position
newPosition (Keys Down _    _    _   ) (Position x y) = Position {x = x - 1, y = y     }
newPosition (Keys _    Down _    _   ) (Position x y) = Position {x = x + 1, y = y     }
newPosition (Keys _    _    Down _   ) (Position x y) = Position {x = x    , y = y + 1 }
newPosition (Keys _    _    _    Down) (Position x y) = Position {x = x    , y = y - 1 }
newPosition (Keys _    _    _    _   ) (Position x y) = Position {x = x    , y = y     }

playerDraw :: GTA -> IO Picture
playerDraw game = do image <- loadBMP ("./sprites/" ++ playerSpriteFile d (keys (player game)) (playerSprite (player game)))
                     return (translate x y $ image)
                     where Position x y = getPos (player game)
                           d            = playerDirection (player game)

playerSpriteFile :: Direction -> Keys -> SpriteType -> String
playerSpriteFile d (Keys left up right down) s | left == Up && right == Up && up == Up && down == Up = show d ++ "w1.bmp"
                                               | otherwise = show d ++ show s ++ ".bmp"

render :: GTA -> IO Picture
render game = return (pictures (blockList ++ carsList ++ personList)) <> playerDraw game
  where blockList = map block (blocks game)
        carsList = map car (cars game)
        personList = map person (people game)

handleKeys :: Event -> GTA -> IO GTA
handleKeys (EventKey (SpecialKey KeyUp)    s _ _) = updateKeyState (Up, Up, s , Up, North)
handleKeys (EventKey (SpecialKey KeyDown)  s _ _) = updateKeyState (Up, Up, Up, s , South)
handleKeys (EventKey (SpecialKey KeyLeft)  s _ _) = updateKeyState (s , Up, Up, Up, West )
handleKeys (EventKey (SpecialKey KeyRight) s _ _) = updateKeyState (Up, s , Up, Up, East )
handleKeys (EventKey (Char 'p')            Down  _ _) = changeGameState
handleKeys (EventKey (Char 'c')            Down  _ _) = enterCar
handleKeys (EventKey (Char 'r')            Down  _ _) = return . return initialState
handleKeys _                                          = return

enterCar :: GTA -> IO GTA
enterCar game = if playerWidth (player game) == 10
                then enterCar' game
                else makeCar (player game) game

makeCar :: Player -> GTA -> IO GTA
makeCar (Player(Position x' y') k c d h w s) game = return game { cars = (newCars game), player = (newPlayer game) }
  where newCars game = car : (cars game)
        car = Car { carPosition = Position {x = x', y = y'}, carColor = c, carDirection = d, velocity = 0 }
        newPlayer game = Player { playerWidth = 10, playerHeight = 10, playerColor = red,
                                  playerDirection = d, playerPosition = Position { x = (x' + 15),
                                  y = y'}, keys = k, playerSprite = s }

enterCar' :: GTA -> IO GTA
enterCar' game =
  if isJust carIndex'
  then updateCars (setDimensions (player game) (width car) (height car) (carColor car) game)
  else return game
    where carIndex' = elemIndex (False) (closeCars (player game) carsGame)
          carIndex = fromJust carIndex'
          car = carsGame !! carIndex
          updateCars game = return game { cars = newCars }
          newCars = take carIndex carsGame ++ drop (1 + carIndex) carsGame
          carsGame = cars game

setDimensions :: Player -> Float -> Float -> Color -> GTA -> GTA
setDimensions player@(Player {playerHeight, playerWidth, playerColor,
                              playerDirection, playerPosition, keys, playerSprite}) x y c game =
  game { player = Player {
  playerHeight = x, playerWidth = y, playerColor = c, playerDirection, playerPosition, keys, playerSprite} }

closeCars :: Player -> [Car] -> [Bool]
closeCars p c = map (canMove p) c'
  where c' = [[x] | x <- c]

changeGameState :: GTA -> IO GTA
changeGameState game = case gameState game of
  Running -> return game { gameState = Paused }
  _       -> return game { gameState = Running }

update :: Float -> GTA -> IO GTA
update secs game = do
  rnd <- randomNr
  case gameState game of
    Loading -> loading game
    Paused  -> return game
    Running -> return ( updateTraffic rnd (updatePlayerPosition game { elapsedTime = (elapsedTime game) + secs }))

loading :: GTA -> IO GTA
loading game = do x <- readWorld
                  return game { cars = cars x, blocks = blocks x, people = people x, gameState = Running }

randomNr :: IO Int
randomNr = getStdRandom (randomR (0,1))

updateTraffic :: Int -> GTA -> GTA
updateTraffic x game = updatePeople (people game) (updateCars (cars game) game x)
