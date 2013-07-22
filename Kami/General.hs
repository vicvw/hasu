module General
    ( Context   (..)
    , Canvas
    , Drawable  (..)
    , Coord     (..)
    , Value     (..)
    , d, f, h, w

    , chunk
    ) where


import qualified Data.Map as M


instance Drawable d => Drawable [d] where
    points = M.unions . map points


-- instance Drawable Context where
--     points = canvas


class Drawable d where
    points :: d -> Canvas


data Context = Context
   { dimensions :: (Integer, Integer)
   , canvas     :: Canvas }


type Canvas = M.Map Coord Value


instance Show Value where
    show v = case v of
        D c -> [c, c]
        H c -> [c, ' ']
        F s -> s
        W c -> [c]


instance Ord Coord where
    (Coord x1 y1) `compare` (Coord x2 y2) =
        (-y1, x1) `compare` (-y2, x2)


data Coord = Coord
     Integer
     Integer
     deriving Eq


data Value
   = D Char
   | F String
   | H Char
   | W Char


d, h :: Char -> Value
d = D
h = H
w = W


f :: String -> Value
f s = if length s == 2
    then F s
    else error "f: length string /= 2"


chunk :: Integer -> [a] -> [[a]]
chunk n
    = foldr ((:) . take n') []
    . takeWhile (not . null)
    . iterate (drop n')

    where
    n' = fromIntegral n
