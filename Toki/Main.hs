module Main (main) where


import Dzen.Color           (grey)
import Dzen.Dzen            (dzen, runDzen, (<|>), DzenOption (..))
import Dzen.Misc            (screenSize, ScreenSize (..))

import Control.Concurrent   (forkIO, threadDelay)
import Control.Monad        (forever, void, when)

import Data.Time.Clock      (getCurrentTime, utctDayTime)
import Data.Time.Format     (formatTime)
import Data.Time.LocalTime  (getTimeZone, localTimeOfDay, timeToTimeOfDay
                            ,todHour, todMin, todSec, utcToLocalTime, LocalTime)

import System.Locale        (defaultTimeLocale)
import System.Process       (readProcess)


main :: IO ()
main = callibrate $ everyQuarter notify
-- main = notify =<< getTime


notify :: LocalTime -> IO ()
notify time = do
    let fmt   = formatTime defaultTimeLocale "%H時%M分" time
        diff  = localTimeOfDay time
        isDay = todHour diff `elem` [8..21]

    -- forkIO . when isDay $
    --     let n  = todMin diff `div` 15
    --         n' = if n == 0 then 4 else n
    --     in playSound n'

    display fmt


everyQuarter :: (LocalTime -> IO ()) -> IO ()
everyQuarter action = forever $ do
    time <- getCurrentTime
    zone <- getTimeZone time

    let local   = utcToLocalTime zone time
        tod     = localTimeOfDay local
        minutes = todMin tod
        seconds = truncate $ todSec tod

    forkIO $ do
        when (isQuarter minutes) $ action local
        print minutes

    if outOfSync seconds
    then main
    else threadDelaySec 60

    where
    isQuarter = (== 0) . (`mod` 15)
    outOfSync = (/= 0)


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


getTime :: IO LocalTime
getTime = do
    time <- getCurrentTime
    zone <- getTimeZone time

    return $ utcToLocalTime zone time


threadDelaySec :: Integer -> IO ()
threadDelaySec = threadDelay . (* 10^6) . fromIntegral


-- notifySend :: String -> IO ()
-- notifySend summary = void $ readProcess
--     "notify-send" (concat [[summary], bg, fg]) ""

--     where
--     bg = ["-h", "string:bgcolor:#ffffff"]
--     fg = ["-h", "string:fgcolor:#000000"]


display :: String -> IO ()
display time = do
    screen <- screenSize 0
    runDzen $ "echo " ++ time <|> dzenTime screen

    where
    dzenTime :: ScreenSize -> String
    dzenTime (ScreenSize swidth sheight) = dzen
        [ Timeout     5
        , Height      height
        , Width       width
        -- , XPosition   $ halve swidth - halve width
        , XPosition   1715
        -- , YPosition   $ halve sheight - halve height
        , YPosition   5
        , Background  $ grey 0
        , Foreground  $ grey 255
        , Font        "Ume Plus P Gothic-20"
        ]

        where
        height = 100
        width  = 200
        halve  = (`div` 2)


playSound :: Int -> IO ()
playSound n = void $ readProcess
    "mplayer" [ "/usr/share/sounds/freedesktop/stereo/complete.oga"
              , "-loop", show n ] ""
