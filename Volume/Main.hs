module Main where


import Dzen.Color
import Dzen.Dzen
import Dzen.Gdbar
import Dzen.Misc

import Omo    (different, 風, 空)

import Volume (toggleMute, isMuted, volume, decVolume, incVolume)


import System.Environment (getArgs)
import System.Exit        (exitSuccess)
import System.IO.Unsafe   (unsafePerformIO)


main :: IO ()
main = do
    args <- getArgs

    case args of
        ["-"] -> decVolume
        ["+"] -> incVolume
        ["%"] -> toggleMute >> exitSuccess
        ["m", mt, mf] -> do
            m <- isMuted
            putStrLn $ if m then mt else mf
            exitSuccess
        _ -> do
            print . round =<< volume
            exitSuccess

    m <- isMuted
    v <- volume

    displayVolumeBar v m


displayVolumeBar :: Double -> Bool -> IO ()
displayVolumeBar level muted = do
    scr <- screenSize 0

    let dzen1  = dzenVolume scr
        gdbar1 = gdbarVolume scr muted

    runDzen $ "echo " ++ show (level + 1) <|> gdbar1 <|> dzen1

    -- ++ "& sleep 0.05; xprop -name 'dzen title' -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY 0xbfffffff"
    -- kill the one before


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
gdbarVolume _ muted = gdbar . concat $
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
    segs = unsafePerformIO . different (error "unknown host")
        $ 風 16
        . 空 15
