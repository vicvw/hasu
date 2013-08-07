module Main where


import Volume


import Control.Monad  (replicateM_)


main :: IO ()
main = do
    setVolumeOut 0

    2 `replicateM_` increaseVolumeOut
