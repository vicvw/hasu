module Dzen.Main where


import Dzen
import DzenInstances
import Gdbar
import qualified GdbarInstances as G
import Color
import Misc

import Text.Printf (printf)
import System.Cmd  (system)


main = do
    (ScreenSize width height) <- screenSize 0
    let swidth  = width
        sheight = height
        dwidth  = width `div` 4
        dheight = 75
        gwidth  = dwidth - spacer
        gheight = dheight - spacer
        spacer  = 50
        [dbg, gbg, gfg] = map (`with` grey 0)
                              [ grey 0
                              , grey 25
                              , grey 200 ]

        dzen1 = dzen  [ Timeout    2
                      , Height     dheight
                      , Width      dwidth
                      , XPosition  $ (swidth `div` 2) - (dwidth `div` 2)
                      -- , YPosition  $ sheight - 200
                      , YPosition  $ sheight `div` 2 - (dheight `div` 2)
                      , Background dbg
                      ]

        gdbar1 = gdbar . concat $
            [ gwidth |*| gheight
            , gfg `on` gbg
            , segmented gwidth 16 2
            ]

    let string = "echo '50'" <|> gdbar1 <|> dzen1

    putStrLn string
    system string
