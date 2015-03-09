module Main (main) where


import Dropbox  (getFilestatus, Filestatus (..))

import Control.Exception  (catch, SomeException)
import System.Environment (getArgs)


main :: IO ()
main = do
    utd:_ <- getArgs
    (putStrLn . encode utd =<< getFilestatus "")
        `catch` (const $ putStrLn "無" :: SomeException -> IO ())


encode :: String -> Filestatus -> String
encode utd status = case status of
    UpToDate  -> utd
    Syncing   -> "中"
    _         -> "非"
