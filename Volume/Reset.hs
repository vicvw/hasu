module Main (main) where


import Omo    (風, 空)
import Omo.IO (differentIO)

import Volume (decVolume, incVolume, setVolume)


import Control.Monad  (replicateM_)


main :: IO ()
main = differentIO
    $ 風 (do
        setVolume 0
        replicateM_ 4 incVolume)
    . 空 (do
        setVolume 100
        replicateM_ 6 decVolume)
