module Pulse.Main where


import qualified Volume as V


main = do
    -- m1 <- V.isMuted
    -- print m1

    -- V.toggleMute

    -- m2 <- V.isMuted
    -- print m2

    -- v1 <- V.getLinearVolume
    -- print v1

    -- V.increaseVolume

    -- v2 <- V.getVolume
    -- print $ V.linearize v2

    v <- V.getLinearVolume
    print v

    m <- V.isMuted
    print m
