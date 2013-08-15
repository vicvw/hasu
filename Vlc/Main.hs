module Main where


import Vlc


import Data.Maybe (fromJust, fromMaybe)


main :: IO ()
main = do
    meta <- fmap fromJust query

    mapM_ ($ meta)
        [ print . _status
        , putStrLn . _url
        , putStrLn . fromMaybe "無" . _title
        , putStrLn . fromMaybe "無" . _nowPlaying
        ]
