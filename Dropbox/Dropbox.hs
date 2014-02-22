module Dropbox
    ( getFilestatus
    , getFilestatusP
    , getFilestatusManyP
    , getWithStatus
    , getWithStatusR
    , toDropboxPath
    , Filestatus (..)
    ) where


import Control.Applicative    ((<$>))
import Control.Exception.Base (bracket)
import Control.Monad          (filterM)

import Network.Socket

import System.Directory       (doesDirectoryExist, doesFileExist, getDirectoryContents, getHomeDirectory)
import System.FilePath        ((</>))

import Text.ParserCombinators.Parsec


getWithStatusR :: FilePath -> Filestatus -> IO [FilePath]
getWithStatusR origin status = do
    inOrigin <- getFiles =<< origin `getWithStatus` status
    getWithStatusR' inOrigin origin

    where

    getWithStatusR' :: [FilePath] -> FilePath -> IO [FilePath]
    getWithStatusR' acc path = do
        withStatus      <- path `getWithStatus` status
        filesWithStatus <- getFiles withStatus
        dirsWithStatus  <- getDirectories withStatus

        let acc' = acc ++ filesWithStatus

        if null dirsWithStatus
        then return acc'
        else fmap concat . mapM (getWithStatusR' acc') $ dirsWithStatus

    getFiles       = filterM doesFileExist
    getDirectories = filterM doesDirectoryExist


getWithStatus :: FilePath -> Filestatus -> IO [FilePath]
getWithStatus path status =
    map fst . filter ((== status) . snd) <$>
         getFilestatusManyP path


getFilestatusManyP :: String -> IO [(FilePath, Filestatus)]
getFilestatusManyP path = mapM getFilestatusP =<< ls
    where
    ls = mapM (toDropboxPathRel path) =<<
            filter (not . (`elem` [".", ".."])) <$>
                (getDirectoryContents =<< toDropboxPath path)


getFilestatusP :: FilePath -> IO (FilePath, Filestatus)
getFilestatusP path = (,) path <$> getFilestatus path


getFilestatus :: FilePath -> IO Filestatus
getFilestatus path = do
    dbPath <- toDropboxPath path

    withDropbox after . flip send $ unlines
        [ "icon_overlay_file_status"
        , "path\t" ++ dbPath
        , "done"
        ]

    where
    after = either (error . show) toFilestatus .
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
        "unwatched"  -> Unwatched
        _            -> Other status


toDropboxPathRel :: FilePath -> FilePath -> IO FilePath
toDropboxPathRel origin = toDropboxPath . (origin </>)


toDropboxPath :: FilePath -> IO FilePath
toDropboxPath path = do
    home <- getHomeDirectory
    return $ home </> "Dropbox" </> path


withDropbox :: (String -> a) -> (Socket -> IO b) -> IO a
withDropbox after f = do
    home <- getHomeDirectory
    let dbSocket = home </> ".dropbox" </> "command_socket"

    bracket (conn dbSocket) sClose $ \sock -> do
        f sock
        after <$> recvUntil sock "done"

    where
    conn addr = do
        sock <- socket AF_UNIX Stream 0
        connect sock $ SockAddrUnix addr
        return sock

    recvUntil sock line = unlines <$> recvUntil' []
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
    | Unwatched
    | Other String
    deriving (Show, Eq)
