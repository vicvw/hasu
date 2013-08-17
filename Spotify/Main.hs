module Main (main) where


import Spotify


main :: IO ()
main = do
    print =<< isRunning
    print =<< status
    print =<< query

    -- mapM_ ($ meta)
    --     [ print . _artist
    --     , print . _title ]
