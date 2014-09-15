module Main (main) where


import Control.Concurrent (threadDelay)
import Control.Monad      (forever, void)
import System.Process     (readProcess)


main :: IO ()
main = doEveryMin 8 $ xte ["key Control_R"]


xte :: [String] -> IO ()
xte args = void $ readProcess "xte" args ""


doEveryMin, doEverySec, doEvery :: Int -> IO () -> IO ()
doEveryMin m      = doEverySec $ m * 60
doEverySec s      = doEvery    $ s * 10^6
doEvery μ action  = forever $ action >> threadDelay μ
