{-# LANGUAGE LambdaCase#-}

module Main (main) where


import Nara


import Control.Applicative          ((<$>))
import Control.Arrow                ((&&&))
import Control.Monad                (when)

import Data.Time.Calendar.WeekDate  (toWeekDate)
import Data.Time.Clock              (addUTCTime, diffUTCTime, getCurrentTime, NominalDiffTime, UTCTime)
import Data.Time.LocalTime          (getCurrentTimeZone, localDay, localTimeOfDay, todHour, todMin, utcToLocalTime, LocalTime)

import System.Directory             (doesFileExist, removeFile)
import System.Environment           (getArgs)
import System.Process               (system)


main :: IO ()
main = getArgs >>= \case
    [h, m, s] -> do
        now <- getCurrentTime

        toFile
            . (`addUTCTime` now)
            . fromIntegral
            . sum
            . zipWith ($) [hours, mins, secs]
            $ read <$> [h, m, s]

    ["r"] -> removeFile file

    ["m"] -> putStrLn =<< mode

    ["t"] -> doesFileExist file >>= ii
        (do until <- fromFile
            now   <- getCurrentTime

            let diff    = diffUTCTime until now
                (h,m,s) = pretty diff
                ifHour  = fi (h > 0)

            when (diff < 0)
                $ removeFile file
               >> system `mapM_`
                    -- [ "notify-send 殘"
                    [ "mpv --no-terminal /usr/share/sounds/freedesktop/stereo/suspend-error.oga"
                    ]

            putStrLn $ concat
                [ show0 h `ifHour` show0 m
                , "時"    `ifHour` "分"
                , " "
                , show0 m `ifHour` show0 s
                , "分"    `ifHour` "秒"
                ])

        ((=<< getLocalTime)
            $ putStrLn
            . (\(h,m) -> concat
                [ show0 h
                , "時"
                , " "
                , show0 m
                , "分"
                ])
            . (todHour &&& todMin)
            . localTimeOfDay)

    _ -> error "fail"


show0 :: Show a => a -> String
show0 = (. show) $ \case
    [x] -> '0' : [x]
    x   -> x


mode :: IO String
mode = (=<< doesFileExist file) $
    ii (return "殘") weekday


weekday :: IO String
weekday = (<$> getLocalTime)
    $ 曜
    . (\case (_,_,d) -> d)
    . toWeekDate
    . localDay

    where
    曜 = return . ("月火水木金土日" !!) . (subtract 1)


getLocalTime :: IO LocalTime
getLocalTime = do
    z <- getCurrentTimeZone
    t <- getCurrentTime
    return $ utcToLocalTime z t


pretty :: NominalDiffTime -> (Int, Int, Int)
pretty dt = let t = max 0 $ floor dt
    in let (h, r)  = t  `divMod` hours 1
    in let (m, r') = r  `divMod` mins  1
    in let (s, _)  = r' `divMod` secs  1
    in (h, m, s)


toFile :: UTCTime -> IO ()
toFile = writeFile file . show

fromFile :: IO UTCTime
fromFile = read <$> readFile file


file = "/home/v/.殘"


hours, mins, secs :: Num a => a -> a
hours = (* 60) . mins
mins  = (* 60) . secs
secs  = id
