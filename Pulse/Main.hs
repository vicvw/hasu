module Main (main) where


import Volume


main :: IO ()
main = print =<< isMutedApp "spotify"
