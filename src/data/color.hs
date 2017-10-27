{-# LANGUAGE OverloadedStrings #-}
module Data.Color where

import Data.Aeson
import Graphics.Gloss
import Control.Monad

instance FromJSON Color where
  parseJSON (String s) = maybe mzero return $ stringToColor s
  parseJSON _ = mzero

--stringToKeyState :: Text -> Maybe KeyState
stringToColor s
  | s == "red" = Just red
  | s == "blue" = Just blue
  | s == "yellow" = Just yellow
  | otherwise = Nothing
