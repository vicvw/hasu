module Dzen.Color
    ( rgb
    , hex
    , grey
    , red
    , green
    , blue
    , with
    ) where


import Dzen.DzenInstances (Color (..))


rgb :: Integer -> Integer -> Integer -> Color
rgb = RGB


hex :: String -> Color
hex = Hex


grey, red, green, blue :: Integer -> Color
grey  x  = RGB x x x
red   x  = RGB x 0 0
green x  = RGB 0 x 0
blue  x  = RGB 0 0 x


with :: Color -> Color -> Color
(RGB r1 g1 b1) `with` (RGB r2 g2 b2) =
    RGB (r1 + r2) (g1 + g2) (b1 + b2)
