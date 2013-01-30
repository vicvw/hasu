module Dzen
    ( dzen
    , grey
    , red
    , green
    , blue
    , with
    ) where

import DzenInstances

import Data.List   (nub)
import Text.Printf (printf)


dzen :: [DzenOption] -> String
dzen = printf "%s %s" dzenCmd . unwords . map show . nub
  where
    dzenCmd = "dzen2"


grey, red, green, blue :: Integer -> Color
grey  x  = RGB x x x
red   x  = RGB x 0 0
green x  = RGB 0 x 0
blue  x  = RGB 0 0 x


with :: Color -> Color -> Color
(RGB r1 g1 b1) `with` (RGB r2 g2 b2) =
    RGB (r1 + r2) (g1 + g2) (b1 + b2)
