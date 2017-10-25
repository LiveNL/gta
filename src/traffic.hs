module Traffic (car, person) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

car :: Picture
car = translate 0 30 $ color blue $ rectangleSolid 20 30

person :: Picture
person = translate 20 20 $ color green $ rectangleSolid 10 10
