module Ma
    ( doEveryM
    , doEveryS
    , doEvery
    ) where


import Control.Concurrent (threadDelay)
import Control.Monad      (forever)


doEveryM, doEveryS, doEvery :: Int -> IO () -> IO ()
doEveryM m  = doEveryS $ m * 60
doEveryS s  = doEvery  $ s * 1000000
doEvery μ a = forever  $ a >> threadDelay μ
