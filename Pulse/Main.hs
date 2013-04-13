module Pulse.Main where


import qualified Volume as V


-- main = V.getLinearVolume


main = do
    m1 <- V.isMuted
    print m1

    V.toggleMute

    m2 <- V.isMuted
    print m2
