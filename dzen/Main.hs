module Main where

import Dzen
import DzenInstances
import Gdbar
import qualified GdbarInstances as G
import Misc

import Text.Printf (printf)
import System.Cmd  (system)
-- import Control.Monad.Identity
-- import Control.Monad.Reader


main = do
    screen <- screenSize 0
    let swidth  = screenWidth screen
        sheight = screenHeight screen
        dwidth  = 500
        dheight = 50
        gwidth  = dwidth - spacer
        gheight = dheight - spacer
        spacer  = 25
        dbg     = grey 0    --`with` blue 25
        gbg     = grey 25   --`with` blue 25
        gfg     = grey 200  --`with` blue 25

        dzen1 = dzen  [ Timeout    2
                      , Height     dheight
                      , Width      dwidth
                      , XPosition  $ (swidth `div` 2) - (dwidth `div` 2)
                      , YPosition  $ sheight - 200
                      , Background dbg
                      ]

        gdbar1 = gdbar  [ G.Width      gwidth
                        , G.Height     gheight
                        , G.Background gbg
                        , G.Foreground gfg
                        , G.SegmentWidth 20
                        , G.SegmentSpace 1
                        ]

    let string  = "echo '50'" <|> gdbar1 <|> dzen1

    putStrLn string
    system string


infixr 9 <|>
(<|>) :: String -> String -> String
(<|>) = printf "%s | %s"


-- type Gdbar = Reader Dimensions Integer
-- data Dimensions
--     = Dimensions
--     { screenW :: Integer
--     , screenH :: Integer
--     }


-- main = do
--     let gdbar = Dimensions 1920 1080
--     runReader getHeight gdbar
--   where
--     getHeight :: Reader Dimensions Integer
--     getHeight = do
--         height <- asks screenH
--         return height
