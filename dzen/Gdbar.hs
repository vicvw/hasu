module Gdbar
    (gdbar) where

import DzenInstances
import GdbarInstances

import Data.List   (nub)
import Text.Printf (printf)


gdbar :: [GdbarOption] -> String
gdbar = printf "%s %s" gdbarCmd . unwords . map show . nub
  where
    gdbarCmd = "gdbar"
