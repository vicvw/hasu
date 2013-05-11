module Main where


import Spotify


main = do
    meta <- getMetadata

    print . m_artist $ meta
    print . m_title $ meta
