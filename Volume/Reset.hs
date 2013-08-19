module Main where


import Volume


import Control.Monad  (replicateM_)


main :: IO ()
main = do
    setVolumeOut 0

    replicateM_ 2 increaseVolumeOut
