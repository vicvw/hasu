module General
    ( Context   (..)
    , Canvas
    , Drawable  (..)
    , Coord     (..)
    , Value     (..)

    , chunk
    ) where


import qualified Data.Map as M


instance Drawable d => Drawable [d] where
    points = M.unions . map points


class Drawable d where
    points :: d -> Canvas


data Context = Context
     (Integer, Integer)
     Canvas


type Canvas = (M.Map Coord Value)


instance Show Value where
    show v = case v of
        D c     -> [c, c]
        H c     -> [c, ' ']
        F s     -> s


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


chunk :: Integer -> [a] -> [[a]]
chunk n
    = foldr ((:) . take n') []
    . takeWhile (not . null)
    . iterate (drop n')

    where
    n' = fromIntegral n
