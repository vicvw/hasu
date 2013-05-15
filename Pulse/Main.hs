module Main where


import qualified Volume as V


main :: IO ()
main = do
    V.unmute "spotify"
    print =<< V.isMuted "spotify"

    V.unmuteGlobal
    print =<< V.isMutedGlobal
