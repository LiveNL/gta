{-# LANGUAGE OverloadedStrings #-}
module Models.Color where

import Control.Monad
import Data.Aeson
import Graphics.Gloss

instance FromJSON Color where
  parseJSON (String s) = maybe mzero return $ stringToColor s
  parseJSON _          = mzero

stringToColor s
  | s == "blue"   = Just blue
  | s == "red"    = Just red
  | s == "white"  = Just white
  | s == "yellow" = Just yellow
  | otherwise = Nothing
