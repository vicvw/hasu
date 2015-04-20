module Main (main) where


import VolumeNew

import System.Environment (getArgs)


main :: IO ()
main = toggleMuteApp . head =<< getArgs
