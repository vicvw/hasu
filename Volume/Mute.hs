module Main (main) where


import Volume

import System.Environment (getArgs)


main :: IO ()
main = toggleMuteApp . head =<< getArgs
