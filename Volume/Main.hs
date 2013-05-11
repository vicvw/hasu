module Main where


import Dzen.Color
import Dzen.Dzen
import Dzen.Gdbar
import Dzen.Misc

import qualified Volume as V

import System.Environment (getArgs)


main = do
    args <- getArgs
    let cmd = head args

    case cmd of
        "-" -> V.decreaseVolume
        "+" -> V.increaseVolume
        "%" -> V.toggleMute
        _   -> error "invalid argument"

    volume <- V.getLinearVolume
    muted  <- V.isMuted

    displayVolumeBar volume muted


displayVolumeBar :: V.Level -> Bool -> IO ()
displayVolumeBar level muted = do
    scr <- screenSize 0

    let dzen1  = dzenVolume scr
        gdbar1 = gdbarVolume scr muted

    runDzen $ "echo " ++ show (level + 1) <|> gdbar1 <|> dzen1


dzenVolume :: ScreenSize -> String
dzenVolume (ScreenSize swidth sheight) = dzen
    [ Timeout     1
    , Height      height
    , Width       width
    , XPosition   0
    , YPosition   $ sheight - height
    , Background  $ grey 0

    -- , XPosition  $ (swidth `div` 2) - (width `div` 2)
    -- , YPosition  $ (sheight `div` 2) - (height `div` 2)
    ]

    where
    height = 30
    width  = swidth


gdbarVolume :: ScreenSize -> Bool -> String
gdbarVolume (ScreenSize swidth sheight) muted = gdbar . concat $
    [ width |*| height
    , color
    , segmented width 16 2
    ]

    where
    height = 20
    width  = swidth `div` 4
    color  = if muted
             then grey 50 `on` grey 25
             else grey 250 `on` grey 25
