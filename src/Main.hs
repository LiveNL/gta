{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Data.List.Split

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
offset = 0

-- Functions
main :: IO ()
main = playIO window (dark $ dark green) 40 initialState render handleKeys update

window :: Display
window = FullScreen

initialState :: GTA
initialState = Game
  { player = Player {
      playerPosition  = Position { x = 480, y = 250 },
      keys            = Keys { left = Up, right = Up, up = Up, down = Up },
      playerDirection = North, playerWidth = 10, playerHeight = 10,
      playerSprite    = Sprite { spriteType = "player1", spriteState = 1 }, playerVelocity = 0, playerState = Walking, points = 0
    },
    cars = [], people = [], blocks = [], gameState = Loading, elapsedTime = 0, highscore = 0
  }

updateKeyState :: (KeyState, KeyState, KeyState, KeyState, Direction) -> GTA -> IO GTA
updateKeyState (left', right', up', down', d') game@Game{player} = return (game { player = (updateKeyState' player) })
  where updateKeyState' player@Player{keys, playerDirection} = player { keys = keys', playerDirection = d'}
        keys' = Keys { left = left', right = right', up = up', down = down' }

updatePlayerPosition :: GTA -> GTA
updatePlayerPosition game@Game{player}
  | canMove 1 player (cars game) && ((playerState player) == Driving) = updatePoints game -- collision
  | canMove 1 player (cars game) || canMove 1 player (people game) = game -- collision
  | canMove 4 player blocks' = game { player = (updatePlayerPosition' player) }
  | otherwise = game
  where updatePlayerPosition' player@Player{playerPosition, playerVelocity, playerSprite} = player { playerPosition = (fst newPosition'),
                                                                                                     playerVelocity = (snd newPosition'),
                                                                                                     playerSprite = playerSprite {
                                                                                                       spriteType = spriteType playerSprite,
                                                                                                       spriteState = sprite }
                                                                                                   }
        blocks' = moveBlocks (blocks game) [Sidewalk, Road, Wall]
        newPosition' = newPosition (keys player) (getPos player)
        sprite | mod' (roundDecimals (elapsedTime game) 2) 0.5 == 0 = nextSprite (playerSprite player)
               | otherwise = spriteState (playerSprite player)

newPosition :: Keys -> Position -> (Position, Float)
newPosition (Keys Down _    _    _   ) (Position x y) = (Position {x = x - 2, y = y     }, 1)
newPosition (Keys _    Down _    _   ) (Position x y) = (Position {x = x + 2, y = y     }, 1)
newPosition (Keys _    _    Down _   ) (Position x y) = (Position {x = x    , y = y + 2 }, 1)
newPosition (Keys _    _    _    Down) (Position x y) = (Position {x = x    , y = y - 2 }, 1)
newPosition (Keys _    _    _    _   ) (Position x y) = (Position {x = x    , y = y     }, 0)

playerDraw :: [(String,Picture)] -> GTA -> Picture
playerDraw images game = draw images (player game)

list = "./sprites/car1_1.bmp,./sprites/car2_1.bmp,./sprites/car3_1.bmp,./sprites/person1_1.bmp,./sprites/person1_2.bmp,./sprites/person1_3.bmp,./sprites/person2_1.bmp,./sprites/person2_2.bmp,./sprites/person2_3.bmp,./sprites/player1_1.bmp,./sprites/player1_2.bmp,./sprites/player1_3.bmp,./sprites/road_1.bmp,./sprites/sidewalk_1.bmp"

render :: GTA -> IO Picture
render game = do images <- sequence $ map loadBMP names
                 let images' = zip names images
                 return (scale 5 5 (translate (- x) (- y) (pictures (
                   (map (block images') (blocks game)) ++ (map (car images') (cars game)) ++
                   (map (person images') (people game)) ++ [(playerDraw images' game)] ++ [pointsText]))))
 where names = splitOn "," list
       pointsText = drawPoints game
       Position x y = getPos (player game)

updatePoints :: GTA -> GTA
updatePoints game@Game{player} = game { player = (updatePoints' player) }
  where updatePoints' player@Player{points} = player { points = (points + 1)}

drawPoints :: GTA -> Picture
drawPoints game = translate (x + 5) (y + 5) $ scale (0.2) (0.2) $ color black $ text (show (highscore game))
  where Position x y = getPos (player game)
        points' = (points (player game)) `div` 10

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
enterCar game = if playerState (player game) == Walking
                then enterCar' game
                else makeCar (player game) game

makeCar :: Player -> GTA -> IO GTA
makeCar (Player(Position x' y') k d h w s v _ p) game = return game { cars = (newCars game), player = (newPlayer game) }
  where newCars game = car : (cars game)
        car = Car { carPosition = Position {x = x', y = y'}, carSprite = s, carDirection = d, velocity = 0 }
        newPlayer game = Player { playerWidth = 10, playerHeight = 10, playerDirection = d,
                                  playerPosition = Position { x = (x' + 15),
                                  y = y'}, keys = k, playerSprite = Sprite { spriteType = "player1", spriteState = 1 }, playerVelocity = v, playerState = Walking, points = p }

enterCar' :: GTA -> IO GTA
enterCar' game =
  if isJust carIndex'
  then updateCars (game { player = updatedPlayer })
  else return game
    where carIndex' = (elemIndex True (closeCars (player game) carsGame))
          carIndex = fromJust carIndex'
          car = carsGame !! carIndex
          updateCars game = return game { cars = newCars }
          newCars = take carIndex carsGame ++ drop (1 + carIndex) carsGame
          carsGame = cars game
          updatedPlayer = setDimensions (player game) (width car) (height car) (carSprite car) (getDir car) game

setDimensions :: Player -> Float -> Float -> Sprite -> Direction -> GTA -> Player
setDimensions player@(Player {playerHeight, playerWidth, playerSprite, playerDirection}) x y s d game =
  player {playerWidth = x, playerHeight = y, playerSprite = s, playerState = Driving, playerDirection = d}

closeCars :: Player -> [Car] -> [Bool]
closeCars p c = map (canMove 1 p ) c'
  where c' = [[x] | x <- c]

changeGameState :: GTA -> IO GTA
changeGameState game = case gameState game of
  Running -> return game { gameState = Paused }
  _       -> return game { gameState = Running }

update :: Float -> GTA -> IO GTA
update secs game = do
  rInt <- randomNr
  case gameState game of
    Loading -> loading game
    Dead    -> return game
    Paused  -> return game
    Running -> return ( updateTraffic rInt (updatePlayerPosition game { elapsedTime = (elapsedTime game) + secs }))

loading :: GTA -> IO GTA
loading game = do x <- readWorld
                  return game { cars = cars x, blocks = blocks x, people = people x, highscore = highscore x, gameState = Running }

randomNr :: IO Int
randomNr = getStdRandom (randomR (0,1))

updateTraffic :: Int -> GTA -> GTA
updateTraffic rInt game = (updatePeople (people game) rInt ) $ (updateCars (cars game) rInt game)
