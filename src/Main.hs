{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Fixed (mod')
import Data.List
import Data.List.Split
import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game

import Helpers

import Models.Block
import Models.Car
import Models.Game
import Models.Person
import Models.Player
import Models.Position

import System.Random

-- GAME settings
fps :: Int
fps = 60

main :: IO ()
main = playIO window black fps initialState render handleKeys update

window :: Display
window = FullScreen

initialState :: GTA
initialState = Game
  { player = Player {
      playerPosition  = Position { x = 450, y = 350 },
      keys            = Keys { left = Up, right = Up, up = Up, down = Up },
      playerDirection = North, playerWidth = 10, playerHeight = 10,
      playerSprite    = Sprite { spriteType = "player1", spriteState = 1 }, playerVelocity = 0, playerState = Walking, points = 0
    },
    cars = [], people = [], blocks = [], gameState = Init, elapsedTime = 0, highscore = 0, timeLeft = 65, coinCount = (0,10)
}

-- KEY updates
handleKeys :: Event -> GTA -> IO GTA
handleKeys (EventKey (SpecialKey KeyUp)    s     _ _) = updateKeyState (Up, Up, s , Up, North)
handleKeys (EventKey (SpecialKey KeyDown)  s     _ _) = updateKeyState (Up, Up, Up, s , South)
handleKeys (EventKey (SpecialKey KeyLeft)  s     _ _) = updateKeyState (s , Up, Up, Up, West )
handleKeys (EventKey (SpecialKey KeyRight) s     _ _) = updateKeyState (Up, s , Up, Up, East )
handleKeys (EventKey (Char 's')            Down  _ _) = changeGameState 's'
handleKeys (EventKey (Char 'p')            Down  _ _) = changeGameState 'p'
handleKeys (EventKey (Char 'c')            Down  _ _) = enterOrLeaveCar
handleKeys (EventKey (Char 'r')            Down  _ _) = changeGameState 'r'
handleKeys _                                          = return

updateKeyState :: (KeyState, KeyState, KeyState, KeyState, Direction) -> GTA -> IO GTA
updateKeyState (left', right', up', down', d') game@Game{player}
  | gameState game == Running = return (game { player = (updateKeyState' player) })
  | otherwise                 = return game
  where updateKeyState' player = player { keys = keys', playerDirection = d'}
        keys'                  = Keys   { left  = left',
                                          right = right',
                                          up    = up',
                                          down  = down' }

-- PICTURES
render :: GTA -> IO Picture
render game | gameState game == Paused || gameState game == Init = mainScreen
            | otherwise = do names         <- (readFile "./config/sprites.txt")
                             images        <- mapM loadBMP (lines names)
                             screenSize    <- getScreenSize
                             statePicture' <- statePicture (gameState game)
                             let images'    = zip (lines names) images
                             return (scale 5 5 (translate (- x) (- y) (pictures (
                               (map (draw images') (blocks' game)) ++
                                 (map (draw images') (people game)) ++
                                   (map (draw images') (cars game)) ++
                                     [(draw images' (player game))] ++
                                       [drawTimer game screenSize] ++
                                         [drawPoints game screenSize] ++
                                           [translate x (y + 50) $ scale (0.2) (0.2) $ statePicture']))))
 where Position x y = getPos (player game)
       blocks' game = moveBlocks (blocks game) [Sidewalk, Road, Tree, Building, Coin]

mainScreen :: IO Picture
mainScreen = do image <- loadBMP "./sprites/logo.bmp"
                (x, y) <- getScreenSize
                let i = translate 0 (fromIntegral y / 4) $ image
                let keyInfo = [text' (0, 80  ) "S: Start game",
                               text' (0, 40  ) "P: Pause game",
                               text' (0, 0   ) "R: Restart level",
                               text' (0, -40 ) "C: Enter/exit nearby car",
                               text' (0, -80 ) "Arrow keys: movement",
                               text' (0, -140) "Collect all coins before the timer runs out,",
                               text' (0, -180) "but watch out, hostile cars will drive you over!"]
                return (pictures ([i] ++ [(translate (-385) 0 (pictures keyInfo))]))

text' :: (Float, Float) -> String -> Picture
text' (x, y) s = translate x y $ scale 0.25 0.25 $ color yellow $ text s

statePicture :: GameState -> IO Picture
statePicture GameOver  = loadBMP "./sprites/wasted.bmp"
statePicture Completed = loadBMP "./sprites/completed.bmp"
statePicture Dead      = loadBMP "./sprites/wasted.bmp"
statePicture _         = return Blank

drawPoints :: GTA -> (Int, Int) -> Picture
drawPoints game (x, y) = translate (fromIntegral (-topLeftX) + x') (fromIntegral topLeftY + y') $ scale 0.05 0.05 $ pictures [rectangle, score]
  where Position x' y' = getPos (player game)
        topLeftX       = (x `div` 5 `div` 2) - 2
        topLeftY       = (y `div` 5 `div` 2) - 8
        score          = text ("$" ++ show (points (player game)) ++ " (" ++ show (highscore game) ++ ")" ++ cccnt)
        cccnt          = " -- Coins: " ++ show (fst (coinCount game)) ++ "/" ++ show (snd (coinCount game))
        rectangle      = pictures [ translate 1000 45 $ color black $ rectangleSolid 2020 180,
                                    translate 1000 45 $ color white $ rectangleSolid 2000 160 ]

drawTimer :: GTA -> (Int, Int) -> Picture
drawTimer game (x, y) = translate (fromIntegral (topLeftX) + x') (fromIntegral topLeftY + y') $ scale 0.05 0.05 $ pictures [rectangle, score]
  where Position x' y' = getPos (player game)
        topLeftX  = (x `div` 5 `div` 2) - 60
        topLeftY  = (y `div` 5 `div` 2) - 8
        score     = timeLeftText game
        rectangle = pictures [ translate 550 45 $ color black $ rectangleSolid 1150 180,
                               translate 550 45 $ color white $ rectangleSolid 1130 160 ]

timeLeftText :: GTA -> Picture
timeLeftText game = text ("Time left: " ++ min ++ ":" ++ sec)
  where count              = timeLeft game - elapsedTime game
        secCalc            = floor (mod' count (fromIntegral fps))
        sec | secCalc < 10 = "0" ++ show secCalc
            | otherwise    = show secCalc
        minCalc            = floor (count / (fromIntegral fps))
        min | minCalc < 10 = "0" ++ show minCalc
            | otherwise    = show minCalc

-- GAME updates
update :: Float -> GTA -> IO GTA
update secs game@Game{player} = do
  rInt <- randomNr
  case gameState game of
    Dead      -> return game { player = killPlayer player }
    Running   -> writeHighscore (return (timeUp (updateCoins elapsedTime' (updateTraffic rInt (updatePlayerPosition game { elapsedTime = elapsedTime' + secs })))))
    Loading   -> loading game
    _         -> return game
  where elapsedTime' = elapsedTime game

timeUp :: GTA -> GTA
timeUp game | (timeLeft game - elapsedTime game) <= 1 = game {gameState = GameOver }
            | otherwise                               = game

enterOrLeaveCar :: GTA -> IO GTA
enterOrLeaveCar game | playerState (player game) == Walking = return (enterCar game)
                     | otherwise                                 = return (leaveCar game)

changeGameState :: Char -> GTA -> IO GTA
changeGameState c game | c == 's'  = case gameState game of
                                      Init   -> return game { gameState = Loading }
                                      Paused -> return game { gameState = Running }
                                      _      -> return game
                       | c == 'r'  = case gameState game of
                                      Init    -> return game { gameState = Init }
                                      _       -> return game { gameState = Loading }
                       | otherwise = case gameState game of
                                      Running -> return game { gameState = Paused }
                                      Paused  -> return game { gameState = Running }
                                      Init    -> return game { gameState = Loading }
                                      _       -> return game

loading :: GTA -> IO GTA
loading game = do x <- readWorld
                  return initialState { cars = cars x, blocks = blocks x, people = people x, highscore = highscore x, gameState = Running }

randomNr :: IO Int
randomNr = getStdRandom (randomR (0,2))

updateTraffic :: Int -> GTA -> GTA
updateTraffic rInt game = updatePeople (people game) rInt $ updateCars (cars game) rInt game

updateCoins :: Float -> GTA -> GTA
updateCoins rInt game@Game{blocks} = game { blocks = (blocks'' ++ coins') }
  where sprite' coin       = Sprite { spriteType = spriteType (blockSprite coin), spriteState = spriteState' coin }
        coins'             = map updateSprite (coins game)
        updateSprite coin' = coin' { blockSprite = sprite' coin' }
        blocks''           = filter (\x -> blockType x /= Coin) blocks
        spriteState' coin  | mod' (roundDecimals (elapsedTime game) 3) 0.125 == 0 = nextSprite (blockSprite coin)
                           | otherwise                                            = spriteState (blockSprite coin)

-- TODO: PLAYERSTUFF
updatePlayerPosition :: GTA -> GTA
updatePlayerPosition game@Game{player}
  | canMove 1 player (cars game)         && ((playerState player) == Driving) = game
  | canMove 1 player (cars game)         && ((playerState player) == Driving) = updatePoints game
  | any canMove'' (livingPeople game)    && ((playerState player) == Driving) = updatePoints (killPerson game)
  | any canMove'' (coins game)                                                = removeCoin (updatePoints game)
  | canMove 1 player (cars game)                                              = game
  | canMove 1 player (livingPeople game)                                      = game
  | canMove 4 player (blocks' game)                                           = game { player = (updatePlayerPosition' player elapsedTime') }
  | otherwise = game
  where elapsedTime' = elapsedTime game
        canMove'' x  = canMove 1 x [player]

blocks' :: GTA -> [Block]
blocks' game = moveBlocks (blocks game) [Sidewalk, Road, Wall, Tree]

coins :: GTA -> [Block]
coins game = filter (\x -> blockType x == Coin) (blocks game)

livingPeople :: GTA -> [Person]
livingPeople game = filter (\x -> personVelocity x == 1) people'
  where people' = people game

killPerson :: GTA -> GTA
killPerson game = game { people = newPeople }
  where newPeople        = take deadPersonIndex (people game) ++ drop (1 + deadPersonIndex) (people game) ++ [newPerson]
        newPerson        = person { personPosition = (personPosition person), personSprite = sprite, personDirection = North, personVelocity = 0}
        sprite           = Sprite { spriteType = "person2", spriteState =  0 }
        deadPersonIndex' = (elemIndex True (concatMap close' (people game)))
        deadPersonIndex  = fromJust deadPersonIndex'
        person           = (people game) !! deadPersonIndex
        close' c         = close c [(player game)]

removeCoin :: GTA -> GTA
removeCoin game = game { gameState = gameState', blocks = newBlocks, coinCount = ((fst (coinCount game) + 1), (snd (coinCount game)))}
  where newBlocks  = take coinIndex (coins game) ++ drop (1 + coinIndex) (coins game) ++ blocks'
        coinIndex' = (elemIndex True (concatMap close' (coins game)))
        coinIndex  = fromJust coinIndex'
        blocks'    = moveBlocks (blocks game) [Sidewalk, Road, Wall, Tree, Building]
        close' c   = close c [(player game)]
        gameState' | (fst (coinCount game) + 1) == snd (coinCount game) = Completed
                   | otherwise                                    = gameState game

leaveCar :: GTA -> GTA
leaveCar game = game { cars = newCars, player = (carToPlayer (player game)) }
  where newCars        = car : (cars game)
        car            = Car { carPosition = Position { x = x', y = y' }, carSprite = s, carDirection = d, velocity = 0 }
        Position x' y' = getPos (player game)
        s              = playerSprite (player game)
        d              = getDir (player game)

enterCar :: GTA -> GTA
enterCar game =
  if isJust carIndex'
  then updateCars (game { player = (playerToCar (player game) car) })
  else game
    where carIndex'       = (elemIndex True (close (player game) carsGame))
          carIndex        = fromJust carIndex'
          car             = carsGame !! carIndex
          updateCars game = game { cars = newCars }
          newCars         = take carIndex carsGame ++ drop (1 + carIndex) carsGame
          carsGame        = cars game

updateCars :: [Car] -> Int -> GTA -> GTA
updateCars cars rInt game = game { cars = updateCars', gameState = updateGameState }
  where updateCars'     = map snd update
        update          = map (updateCar game rInt) cars
        updateGameState | elem Dead (map fst update) = Dead
                        | otherwise = gameState game

-- TODO: CARSTUFF
updateCar :: GTA -> Int -> Car -> (GameState, Car)
updateCar game rInt car@Car{velocity} = case velocity of
  0 -> (Running, car)
  1 | canMove 4 car walls'                                                     -> (Running, (changeDirR rInt car))
    | canMove 1 car cars'                                                      -> (Running, changeDir 2 car)
    | canMove 1 (player game) [car] && (playerState (player game) == Walking ) -> (Dead, car)
    | canMove 1 car [(player game)]                                            -> (Running, car)
    | canMove 4 car blocks'                                                    -> (Running, newCarPosition car)
    | otherwise                                                                -> (Running, (changeDir rInt car))
         where blocks'  = moveBlocks (blocks game) [Road]
               walls'   = moveBlocks (blocks game) [Wall]
               carIndex = fromJust (elemIndex car (cars game))
               cars'    = take carIndex (cars game) ++ drop (1 + carIndex) (cars game)

-- TODO: PERSONstuff
updatePeople :: [Person] -> Int -> GTA -> GTA
updatePeople people rInt game = game { people = (map (updatePerson game rInt) people) }

updatePerson :: GTA -> Int -> Person -> Person
updatePerson game rInt person
  | personVelocity person == 0     = person
  | canMove 1 person (cars game)   = changeDir rInt person
  | canMove 1 person people'       = changeDirR rInt person
  | canMove 1 person [player game] = changeDirR rInt person
  | canMove 4 person blocks'       = newPersonPosition person { personSprite =
                                       Sprite { spriteType = spriteType (personSprite person), spriteState = sprite' }}
  | otherwise                      = changeDirR rInt person
    where blocks'     = moveBlocks (blocks game) [Sidewalk]
          people'     = take personIndex (people game) ++ drop (1 + personIndex) (people game)
          personIndex = fromJust (elemIndex person (people game))
          sprite'     | mod' (roundDecimals (elapsedTime game) 2) 0.25 == 0 = nextSprite (personSprite person)
                      | otherwise                                           = spriteState (personSprite person)

close :: (Movable a, Movable b) => a -> [b] -> [Bool]
close p c = map (canMove 1 p ) c'
  where c' = [[x] | x <- c]