module Dzen.Gdbar
    ( gdbar
    , (|*|)
    , on
    , segmented
    ) where


import Dzen.DzenInstances  (Color)
import Dzen.GdbarInstances

import Data.List   (nub)
import Text.Printf (printf)


gdbar :: [GdbarOption] -> String
gdbar = printf "%s %s" gdbarCmd . unwords . map show . nub
    where
    gdbarCmd = "gdbar"


(|*|) :: Integer -> Integer -> [GdbarOption]
width |*| height =  [ Width   width
                    , Height  height ]


on :: Color -> Color -> [GdbarOption]
fg `on` bg = [ Foreground fg
             , Background bg ]


segmented :: Integer -> Integer -> Integer -> [GdbarOption]
segmented width steps space =
    let swidth = (width - steps * space)
                 `div` steps
    in  [ SegmentWidth  swidth
        , SegmentSpace  space ]
