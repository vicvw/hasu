module Main where


import Spotify


main = do
    meta <- getMetadata

    mapM_ ($ meta)
        [ print . m_artist
        , print . m_title ]
