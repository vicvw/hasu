module Main where


import Spotify
import Monitor


main = do
    -- meta <- getMetadata
    -- time <- getCurrentTime
    stat <- getPlaybackStatus

    print stat

    -- print meta

    -- mapM_ ($ meta)
    --     [ print . _artist
    --     , print . _title ]

    -- listenForAd meta time
