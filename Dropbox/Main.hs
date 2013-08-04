module Main (main) where


import Dropbox

import System.FilePath  (takeFileName)


main :: IO ()
main = do
    -- files <- "" `getWithStatusR` Syncing

    -- mapM_ putStrLn files
    -- mapM_ (putStrLn . takeFileName) files


    status <- getFilestatus ""
    print status
