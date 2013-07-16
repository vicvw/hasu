module Objects.Point
    ( d, f, h
    , point

    , Point (..)
    ) where


import General

import qualified Data.Map as M  (singleton)


instance Drawable Point where
    points (Point c v) = M.singleton c v


point :: (Integer, Integer) -> Value -> Point
point (x, y) = Point (Coord x y)


d, h :: Char -> Value
d = D
h = H


f :: String -> Value
f s = if length s == 2
    then F s
    else error "f: length string /= 2"


instance Eq Point where
    (Point c1 _) == (Point c2 _) =
        c1 == c2


instance Ord Point where
    (Point c1 _) `compare` (Point c2 _) =
        c1 `compare` c2


data Point = Point
   { coord :: Coord
   , value :: Value }
