module Main (main) where


import Control.Concurrent (threadDelay)
import Control.Monad      (forever, void)
import System.Process     (system)


main :: IO ()
main = doEveryMin 10 . void $ system "sudo pacman -Sy"


doEveryMin, doEverySec, doEvery :: Int -> IO () -> IO ()
doEveryMin m      = doEverySec $ m * 60
doEverySec s      = doEvery    $ s * 10^6
doEvery μ action  = forever $ action >> threadDelay μ
