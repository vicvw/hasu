module Canvas
    ( empty
    , draw
    , render
    ) where


import General

import Data.List  (intercalate)
import qualified Data.Map as M


empty :: (Integer, Integer) -> Value -> Context
empty (width, height) v = Context (width, height) $ M.fromList
    [ (Coord x y, v)
    | y <- [0..height - 1]
    , x <- [0..width - 1]
    ]


render :: Context -> String
render (Context (width, _) cvs)
    = intercalate "\n"
    . map (concatMap (show . snd))
    . chunk width
    . M.toAscList
    $ cvs


draw :: Drawable d => d -> Context -> Context
draw obj (Context dims@(width, height) cvs)
    = Context dims
    . M.filterWithKey (\(Coord x y) _ -> and
          [ x >= 0, x < width
          , y >= 0, y < height ])
    . M.union (points obj)
    $ cvs
