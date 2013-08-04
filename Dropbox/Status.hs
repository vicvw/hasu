module Main (main) where


import Dropbox  (getFilestatus, Filestatus (..))

import Control.Exception


main :: IO ()
main = do
    (putStrLn . encode =<< getFilestatus "")
    `catch`
    (const $ putStrLn "無" :: SomeException -> IO ())



encode :: Filestatus -> String
encode status = case status of
    UpToDate  -> "同"
    Syncing   -> "中"
    _         -> "非"
