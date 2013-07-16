module Main where


import Dzen.Color
import Dzen.Dzen
import Dzen.Gdbar
import Dzen.Misc

import Pulse.Volume

import System.Environment (getArgs)
import System.IO.Unsafe   (unsafePerformIO)


main :: IO ()
main = do
    args <- getArgs
    let cmd = head args

    case cmd of
        "-" -> decreaseVolume
        "+" -> increaseVolume
        "%" -> toggleMuteGlobal
        _   -> error "invalid argument"

    volume <- getLinearVolume
    muted  <- isMutedGlobal

    displayVolumeBar volume muted


displayVolumeBar :: Level -> Bool -> IO ()
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
gdbarVolume (ScreenSize swidth _) muted = gdbar . concat $
    [ width |*| height
    , color
    , segmented width segs 2
    ]

    where
    height = 20
    width  = swidth `div` 4
    color  = if muted
             then grey 50 `on` grey 25
             else grey 250 `on` grey 25
    segs   = case unsafePerformIO getHostname of
                 "kaze"   -> 16
                 "heaven" -> 15
                 _        -> error "illegal host"
