module Main (main) where


import Control.Concurrent (threadDelay)
import Control.Monad      (forever, void)
import System.Process     (readProcess)


main :: IO ()
main = do
    putStrLn "中"
    doEveryMin 8 $ xte ["key Control_R"]


xte :: [String] -> IO ()
xte args = void $ readProcess "xte" args ""


doEverySec s = doEvery    $ s * 10^6
doEveryMin m = doEverySec $ m * 60

doEvery, doEverySec, doEveryMin :: Int -> IO () -> IO ()
doEvery μ action = forever $ action >> threadDelay μ
