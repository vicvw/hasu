module Main (main) where


import Vlc


import Data.Maybe (fromMaybe)


main :: IO ()
main = maybe
    (putStrLn "not running")
    (\meta -> mapM_ ($ meta)
        [ print . _status
        , putStrLn . _url
        , putStrLn . fromMaybe "無" . _title
        , putStrLn . fromMaybe "無" . _nowPlaying
        ])
    =<< query
