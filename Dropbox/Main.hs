module Main where


import Control.Exception.Base (bracket)
import Control.Monad          (forM_)

import Network.Socket

import System.Directory       (getHomeDirectory, getDirectoryContents)
import System.FilePath        ((</>))

import Text.ParserCombinators.Parsec


main :: IO ()
main = do
    paths <- ls ""

    forM_ paths $ \path -> do
        stat <- getFilestatus path
        print (path, stat)

    where
    ls path = do
        dbPath <- dropboxPath ""

        (map (dbPath </>) . filter (not . flip elem [".", ".."])) `fmap`
            (getDirectoryContents =<< dropboxPath path)


getFilestatus :: FilePath -> IO Filestatus
getFilestatus path = withDropbox after . flip send $ unlines
    [ "icon_overlay_file_status"
    , "path\t" ++ path
    , "done"
    ]

    where
    after = either (error "poop") toFilestatus .
                   parse parseFilestatus "filestatus"

    parseFilestatus = do
        string "ok" >> newline
        string "status" >> tab
        manyTill (letter <|> space) newline

    toFilestatus status = case status of
        "up to date" -> UpToDate
        "syncing"    -> Syncing
        "unsyncable" -> Unsyncable
        "selsync"    -> SelectiveSync
        _            -> Other status


dropboxPath :: FilePath -> IO FilePath
dropboxPath path = do
    home <- getHomeDirectory
    return $ home </> "Dropbox" </> path


withDropbox :: (String -> a) -> (Socket -> IO b) -> IO a
withDropbox after f = do
    home <- getHomeDirectory
    let dbSocket = home </> ".dropbox" </> "command_socket"

    bracket (conn dbSocket) sClose $ \sock -> do
        f sock
        after `fmap` recvUntil sock "done"

    where
    conn addr = do
        sock <- socket AF_UNIX Stream 0
        connect sock $ SockAddrUnix addr
        return sock

    recvUntil sock line = unlines `fmap` recvUntil' []
        where
        recvUntil' acc = do
            l <- recvLine

            if l == line
            then return acc
            else recvUntil' $ acc ++ [l]

        recvLine = recvLine' ""
            where
            recvLine' acc = do
                c <- recv sock 1

                if c == "\n"
                then return acc
                else recvLine' $ acc ++ c


data Filestatus
    = UpToDate
    | Syncing
    | Unsyncable
    | SelectiveSync
    | Other String
    deriving Show
