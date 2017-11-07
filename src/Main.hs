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

import Graphics.Gloss.Interface.Environment

-- Variables
windowWidth, windowHeight, offset :: Int
windowWidth  = 600
windowHeight = 600
offset = 0

-- Functions
main :: IO ()
main = playIO window (dark $ dark green) 60 initialState render handleKeys update

window :: Display
window = FullScreen -- InWindow "GTA" (windowWidth, windowHeight) (offset, offset)

initialState :: GTA
initialState = Game
  { player = Player {
      playerPosition  = Position { x = 450, y = 350 },
      keys            = Keys { left = Up, right = Up, up = Up, down = Up },
      playerDirection = North, playerWidth = 10, playerHeight = 10,
      playerSprite    = Sprite { spriteType = "player1", spriteState = 1 }, playerVelocity = 0, playerState = Walking, points = 0
    },
    cars = [], people = [], blocks = [], gameState = Loading, elapsedTime = 0, highscore = 0
  }


updateKeyState :: (KeyState, KeyState, KeyState, KeyState, Direction) -> GTA -> IO GTA
updateKeyState (left', right', up', down', d') game@Game{player} = if gameState game == Running
                                                                      then return (game { player = (updateKeyState' player) })
                                                                      else return game
  where updateKeyState' player = player { keys = keys', playerDirection = d'}
        keys' = Keys { left = left', right = right', up = up', down = down' }

updatePlayerPosition :: GTA -> GTA
updatePlayerPosition game@Game{player}
  | canMove 1 player (cars game)         && ((playerState player) == Driving) = updatePoints game
  | canMove 1 player (livingPeople game) && ((playerState player) == Driving) = killPerson game
  | canMove 1 player (cars game)                                              = game
  | canMove 1 player (livingPeople game)                                      = game
  | canMove 4 player blocks'                                                  = game { player = (updatePlayerPosition' player) }
  | otherwise = game
  where updatePlayerPosition' player@Player{playerSprite} = player { playerPosition = (fst newPosition'),
                                                                     playerVelocity = (snd newPosition'),
                                                                     playerSprite = playerSprite {
                                                                       spriteType = spriteType playerSprite,
                                                                       spriteState = sprite }
                                                                   }
        blocks' = moveBlocks (blocks game) [Sidewalk, Road, Wall, Tree]
        newPosition' = newPosition (keys player) (getPos player)
        sprite = case playerState player of
                   Walking | mod' (roundDecimals (elapsedTime game) 2) 0.5 == 0 -> nextSprite (playerSprite player)
                           | otherwise -> spriteState (playerSprite player)
                   Driving -> spriteState (playerSprite player)

livingPeople :: GTA -> [Person]
livingPeople game = filter (\x -> personVelocity x == 1) people'
  where people' = people game

killPerson :: GTA -> GTA
killPerson game = updatePeople
  where updatePeople = game { people = newPeople }
        newPeople = take deadPersonIndex (people game) ++ drop (1 + deadPersonIndex) (people game) ++ [newPerson]
        newPerson = person { personPosition = (personPosition person), personSprite = sprite, personDirection = North, personVelocity = 0}
        sprite = Sprite { spriteType = "person2", spriteState =  0 }
        deadPersonIndex' = (elemIndex True (deadPerson game))
        deadPersonIndex = fromJust deadPersonIndex'
        person = (people game) !! deadPersonIndex

deadPerson :: GTA -> [Bool]
deadPerson game = map (canMove 1 p ) c'
  where c' = [[x] | x <- c]
        c = people game
        p = player game

newPosition :: Keys -> Position -> (Position, Float)
newPosition (Keys Down _    _    _   ) (Position x y) = (Position {x = x - 2, y = y     }, 1)
newPosition (Keys _    Down _    _   ) (Position x y) = (Position {x = x + 2, y = y     }, 1)
newPosition (Keys _    _    Down _   ) (Position x y) = (Position {x = x    , y = y + 2 }, 1)
newPosition (Keys _    _    _    Down) (Position x y) = (Position {x = x    , y = y - 2 }, 1)
newPosition (Keys _    _    _    _   ) (Position x y) = (Position {x = x    , y = y     }, 0)

list = "./sprites/car1_1.bmp,./sprites/car2_1.bmp,./sprites/car3_1.bmp,./sprites/car4_1.bmp,./sprites/car5_1.bmp,./sprites/car6_1.bmp,./sprites/car7_1.bmp,./sprites/car8_1.bmp,./sprites/person1_1.bmp,./sprites/person1_2.bmp,./sprites/person1_3.bmp,./sprites/person2_1.bmp,./sprites/person2_2.bmp,./sprites/person2_3.bmp,./sprites/player1_1.bmp,./sprites/player1_2.bmp,./sprites/player1_3.bmp,./sprites/road_1.bmp,./sprites/sidewalk_1.bmp,./sprites/building_1.bmp,./sprites/tree1_1.bmp,./sprites/tree2_1.bmp,./sprites/person2_0.bmp,./sprites/player1_0.bmp"

render :: GTA -> IO Picture
render game = do images <- mapM loadBMP names
                 let images' = zip names images
                 screenSize <- getScreenSize
                 return (scale 5 5 (translate (- x) (- y) (pictures (
                   (map (block images') (blocks game)) ++ (map (draw images') (cars game)) ++
                     (map (draw images') (people game)) ++ [(draw images' (player game))] ++ [drawPoints game screenSize]))))
 where names = splitOn "," list
       Position x y = getPos (player game)

updatePoints :: GTA -> GTA
updatePoints game@Game{player} = game { player = (updatePoints' player), highscore = updateHighscore player }
  where updatePoints' player@Player{points} = player { points = (points + 1)}
        updateHighscore player@Player{points} | (points + 1) >= highscore game = (points + 1)
                                              | otherwise                      = highscore game

drawPoints :: GTA -> (Int, Int) -> Picture 
drawPoints game (x, y) = translate (fromIntegral (-topLeftX) + x') (fromIntegral topLeftY + y') $ scale 0.05 0.05 $ pictures [rectangle, score]
  where Position x' y' = getPos (player game)
        topLeftX = (x `div` 5 `div` 2) - 2
        topLeftY = (y `div` 5 `div` 2) - 8
        score =  text ("Score: " ++ show (points (player game)) ++ " (" ++ show (highscore game) ++ ")")
        rectangle = pictures [ translate 740 45 $ color black $ rectangleSolid 1500 180, 
                               translate 740 45 $ color white $ rectangleSolid 1480 160 ]
                               
handleKeys :: Event -> GTA -> IO GTA
handleKeys (EventKey (SpecialKey KeyUp)    s _ _) = updateKeyState (Up, Up, s , Up, North)
handleKeys (EventKey (SpecialKey KeyDown)  s _ _) = updateKeyState (Up, Up, Up, s , South)
handleKeys (EventKey (SpecialKey KeyLeft)  s _ _) = updateKeyState (s , Up, Up, Up, West )
handleKeys (EventKey (SpecialKey KeyRight) s _ _) = updateKeyState (Up, s , Up, Up, East )
handleKeys (EventKey (Char 'p')            Down  _ _) = changeGameState
handleKeys (EventKey (Char 'c')            Down  _ _) = enterOrLeaveCar
handleKeys (EventKey (Char 'r')            Down  _ _) = return . return initialState
handleKeys _                                          = return

enterOrLeaveCar :: GTA -> IO GTA
enterOrLeaveCar game =
  if playerState (player game) == Walking
  then enterCar game
  else leaveCar game

leaveCar :: GTA -> IO GTA
leaveCar game = return game { cars = newCars, player = (carToPlayer (player game)) }
  where newCars = car : (cars game)
        car = Car { carPosition = Position {x = x', y = y'}, carSprite = s, carDirection = d, velocity = 0 }
        Position x' y' = getPos (player game)
        s = playerSprite (player game)
        d = getDir (player game)

enterCar :: GTA -> IO GTA
enterCar game =
  if isJust carIndex'
  then updateCars (game { player = (playerToCar (player game) car) })
  else return game
    where carIndex' = (elemIndex True (closeCars (player game) carsGame))
          carIndex = fromJust carIndex'
          car = carsGame !! carIndex
          updateCars game = return game { cars = newCars }
          newCars = take carIndex carsGame ++ drop (1 + carIndex) carsGame
          carsGame = cars game

carToPlayer :: Player -> Player
carToPlayer player = player {playerWidth = w', playerHeight = h', playerSprite = s', playerPosition = p', playerState = Walking}
  where
    w' = 10
    h' = 10
    s' = Sprite { spriteType = "player1", spriteState = 1 }
    p' = Position { x = (x' + 35), y = y'}
    Position x' y' = getPos player

playerToCar :: Player -> Car -> Player
playerToCar player car =
  player {playerWidth = w', playerHeight = h', playerSprite = s', playerState = Driving, playerDirection = d', playerPosition = p'}
    where
      w' = width car
      h' = height car
      s' = carSprite car
      d' = getDir car
      p' = getPos car

closeCars :: Player -> [Car] -> [Bool]
closeCars p c = map (canMove 1 p ) c'
  where c' = [[x] | x <- c]

changeGameState :: GTA -> IO GTA
changeGameState game = case gameState game of
  Running -> return game { gameState = Paused }
  _       -> return game { gameState = Running }

update :: Float -> GTA -> IO GTA
update secs game@Game{player} = do
  rInt <- randomNr
  case gameState game of
    Loading -> loading game
    Dead    -> return game { player = killPlayer player }
    Paused  -> return game
    Running -> writeJSON ( return ( updateTraffic rInt (updatePlayerPosition game { elapsedTime = (elapsedTime game) + secs })))

killPlayer :: Player -> Player
killPlayer player@Player{} = player { playerSprite = Sprite { spriteType = "player1", spriteState =  2 }, playerVelocity = 2 }

loading :: GTA -> IO GTA
loading game = do x <- readWorld
                  return game { cars = cars x, blocks = blocks x, people = people x, highscore = highscore x, gameState = Running }

randomNr :: IO Int
randomNr = getStdRandom (randomR (0,2))

updateTraffic :: Int -> GTA -> GTA
updateTraffic rInt game = (updatePeople (people game) rInt ) $ (updateCars (cars game) rInt game)