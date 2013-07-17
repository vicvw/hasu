module Dzen.Color
    ( rgb
    , hex
    , grey
    , red
    , green
    , blue
    , with
    , without
    ) where


import Dzen.DzenInstances (Color (..))


rgb :: Integer -> Integer -> Integer -> Color
rgb r g b = RGB r' g' b'

    where
    [r', g', b'] = (bound0 . bound255)
                   `map` [r, g, b]
    bound0   = max 0
    bound255 = min 255


hex :: String -> Color
hex = Hex


grey, red, green, blue :: Integer -> Color
grey  x = rgb x x x
red   x = rgb x 0 0
green x = rgb 0 x 0
blue    = rgb 0 0


with :: Color -> Color -> Color
(RGB r1 g1 b1) `with` (RGB r2 g2 b2) =
    rgb (r1 + r2) (g1 + g2) (b1 + b2)


without :: Color -> Color -> Color
(RGB r1 g1 b1) `without` (RGB r2 g2 b2) =
    rgb (r1 - r2) (g1 - g2) (b1 - b2)
