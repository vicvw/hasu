module Main where


import Hosuto

import System.Environment (getArgs)


main = do
    args <- getArgs

    if length args `mod` 3 /= 0
    then error "% 3"
    else do
        let launchers = map launcherFromList $ chunk 3 args
        hostname <- getHostname

        -- mapM_ (putStrLn . show) launchers
        mapM_ run $ filter ((== hostname) . lHostname) launchers
