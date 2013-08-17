module Main where


import Spotify.General


main :: IO ()
main = do
    -- meta <- query
    -- print meta

    -- mapM_ ($ meta)
    --     [ print . _artist
    --     , print . _title ]

    print =<< isRunning
