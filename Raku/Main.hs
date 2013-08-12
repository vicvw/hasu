module Main (main) where


import qualified Cmus


-- fix kaba


main :: IO ()
main = print =<< Cmus.isRunning
