module Main where


import Pulse.Volume   (increaseVolume, setVolume)

import Control.Monad  (replicateM_)


main :: IO ()
main = do
    setVolume 0

    2 `replicateM_` increaseVolume
