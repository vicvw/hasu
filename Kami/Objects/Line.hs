module Objects.Line
    ( line

    , Line (..)
    ) where


import General

import Data.List  (sort, unfoldr)
import qualified Data.Map as M  (fromList)
-- import qualified Data.Map as M  (empty, fromList, singleton, union)


instance Drawable Line where
    points = makeLine


makeLine :: Line -> Canvas
makeLine (Line (Coord x1 y1) (Coord x2 y2) v)
    = M.fromList
    . map (flip (,) v . uncurry Coord . maySwitch)
    . unfoldr go
    $ (x1', y1', 0)

    where
    steep = abs (y2 - y1) > abs (x2 - x1)

    maySwitch = if steep
        then (\(x, y) -> (y, x))
        else id

    [(x1', y1'), (x2', y2')] = sort [maySwitch (x1, y1), maySwitch (x2, y2)]

    deltax = x2' - x1'
    deltay = abs $ y2' - y1'

    ystep = if y1' < y2'
        then 1
        else -1

    go (xTemp, yTemp, err) = if xTemp > x2'
        then Nothing
        else Just ((xTemp, yTemp), (xTemp + 1, newY, newError))

        where
        tempError = err + deltay

        (newY, newError) = if 2 * tempError >= deltax
            then (yTemp + ystep, tempError - deltax)
            else (yTemp, tempError)


line :: (Integer, Integer) -> (Integer, Integer) -> Value -> Line
line (x1, y1) (x2, y2) = Line (Coord x1 y1) (Coord x2 y2)


data Line = Line
     Coord
     Coord
     Value


-- makeLine :: Line -> Canvas
-- makeLine (Line (Coord x1 y1) (Coord x2 y2) v) =
--     makeLine' y err [x1'..x2'] M.empty

--     where
--     makeLine' y' err' dec acc = case dec of
--         []       -> acc
--         (x : xs) -> let acc' = M.union acc
--                              . flip M.singleton v
--                              $ if isSteep
--                                then Coord y' x
--                                else Coord x y'
--             in if err' < 0
--             then
--                 makeLine' (y' + ystep)
--                           (newErr + dx)
--                           xs
--                           acc'
--             else
--                 makeLine' y'
--                           newErr
--                           xs
--                           acc'

--         where
--         newErr = err' - dy

--     isSteep = abs (y2 - y1) > abs (x2 - x1)

--     (x1', y1', x2', y2')
--         | isSteep   = (y1, x1, y2, x2)
--         | x1 > x2   = (x2, x1, y2, y1)
--         | otherwise = (x1, y1, x2, y2)

--     (dx, dy) = (x2 - x1, abs $ y2 - y1)

--     err   = dx `div` 2
--     y     = y1'
--     ystep = if y1' < y2'
--         then 1
--         else -1
