module Main (main) where


import Dzen.Color           (grey)
import Dzen.Dzen            (dzen, runDzen, (<|>), DzenOption (..))
import Dzen.Misc            (screenSize, ScreenSize (..))

import Control.Concurrent   (forkIO, threadDelay)
import Control.Monad        (forever, void, when)

import Data.Time.Clock      (getCurrentTime, utctDayTime)
import Data.Time.Format     (formatTime)
import Data.Time.LocalTime  (getTimeZone, timeToTimeOfDay, todMin, todSec, utcToLocalTime)

import System.Locale        (defaultTimeLocale)
import System.Process       (readProcess)


main :: IO ()
main = callibrate $ everyQuarter action
-- main = display =<< getTime
-- main = action "16:45" 3

    where
    action time n = do
        forkIO $ display time
        playSound n


everyQuarter :: (String -> Int -> IO ()) -> IO ()
everyQuarter action = forever $ do
    time <- getCurrentTime
    zone <- getTimeZone time

    let local   = utcToLocalTime zone time
        tod     = timeToTimeOfDay . utctDayTime $ time
        minutes = todMin tod
        seconds = truncate $ todSec tod

    putStrLn $ "seconds: " ++ show seconds

    if outOfSync seconds
    then main
    else do
        forkIO $ do
            when (isQuarter minutes) $
                -- action $ formatTime defaultTimeLocale "%R" local
                action (formatTime defaultTimeLocale "%H時%M分" local)
                       (bells minutes)

            print minutes

        threadDelaySec 60

    where
    isQuarter = (== 0) . (`mod` 15)
    outOfSync = (/= 0)
    bells m   = let n = m `div` 15 in
        if n == 0
        then 4
        else n


callibrate :: IO () -> IO ()
callibrate action = do
    seconds <- (truncate . todSec . timeToTimeOfDay . utctDayTime)
               `fmap` getCurrentTime

    forkIO $ print seconds

    if seconds == 0
    then action
    else do
        threadDelaySec 1
        callibrate action


getTime :: IO String
getTime = do
    time <- getCurrentTime
    zone <- getTimeZone time

    let local = utcToLocalTime zone time

    -- return $ formatTime defaultTimeLocale "%R" local
    return $ formatTime defaultTimeLocale "%H時%M分" local


threadDelaySec :: Integer -> IO ()
threadDelaySec = threadDelay . (* 10^6) . fromIntegral


notifySend :: String -> IO ()
notifySend summary = void $ readProcess
    "notify-send" (concat [[summary], bg, fg]) ""
    -- "notify-send" [summary] ""

    where
    bg = ["-h", "string:bgcolor:#ffffff"]
    fg = ["-h", "string:fgcolor:#000000"]


display :: String -> IO ()
display time = do
    screen <- screenSize 0
    runDzen $ "echo " ++ time <|> dzenTime screen

    where
    dzenTime :: ScreenSize -> String
    dzenTime (ScreenSize swidth sheight) = dzen
        [ Timeout     10
        , Height      height
        , Width       width
        , XPosition   $ halve swidth - halve width
        , YPosition   $ halve sheight - halve height
        , Background  $ grey 0
        , Foreground  $ grey 255
        -- , Font        "Roboto-20:bold"
        , Font        "Ume Plus P Gothic-20"
        -- , Font        "FZHei\\-B01-20"
        ]

        where
        height = 100
        width  = 200
        halve  = (`div` 2)


playSound :: Int -> IO ()
playSound n = void $ readProcess
    "mplayer" [ "/usr/share/sounds/freedesktop/stereo/complete.oga"
              , "-loop", show n ] ""
