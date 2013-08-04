module Main where


import Dzen.Color
import Dzen.Dzen
import Dzen.Gdbar
import Dzen.Misc

import Pulse.Volume

import System.Environment (getArgs)
import System.Exit        (exitSuccess)
import System.IO.Unsafe   (unsafePerformIO)


main :: IO ()
main = do
    args <- getArgs
    let cmd = head args

    case cmd of
        "-" -> decreaseVolume
        "+" -> increaseVolume
        "%" -> toggleMuteGlobal
        _   -> do
            print . round =<< getLinearVolume
            exitSuccess

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
    -- , Width       width
    , Width       width
    , XPosition   0
    -- , XPosition   $ swidth `div` 2 - width `div` 2
    , YPosition   $ sheight - height
    , Background  $ grey 0

    -- , XPosition  $ (swidth `div` 2) - (width `div` 2)
    -- , YPosition  $ (sheight `div` 2) - (height `div` 2)
    ]

    where
    height = 20
    width  = swidth
    -- width  = 324


gdbarVolume :: ScreenSize -> Bool -> String
gdbarVolume (ScreenSize _ _) muted = gdbar . concat $
    [ width |*| height
    , color
    , segmented width segs 2
    ]

    where
    height = 15
    -- width  = swidth `div` 6
    width  = 320
    color  = if muted
             then grey 100 `on` grey 50
             else grey 250 `on` grey 50
    -- color  = if muted
    --          then grey 150 `on` grey 200
    --          else grey 0 `on` grey 200
    segs   = case unsafePerformIO getHostname of
                 "kaze"   -> 16
                 "heaven" -> 15
                 _        -> error "illegal host"
