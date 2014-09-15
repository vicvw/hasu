module Main (main) where


import Dropbox

import System.FilePath  (takeFileName)


main :: IO ()
main = do
    files <- "" `getWithStatusR` Syncing
    -- files <- "" `getWithStatusR` Unsyncable

    mapM_ putStrLn files
    -- mapM_ (putStrLn . takeFileName) files


    -- status <- getFilestatus ""
    -- print status
