module Main where


import Volume

import Omo    (風, 空)
import Omo.IO (differentIO)


import Control.Monad  (replicateM_)


main :: IO ()
main = differentIO
    $ 風 (do
        setVolumeOut 0
        replicateM_ 4 increaseVolumeOut)
    . 空 (do
        setVolumeOut 100
        replicateM_ 6 decreaseVolumeOut)
