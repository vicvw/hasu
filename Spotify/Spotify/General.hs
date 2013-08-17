{-# LANGUAGE OverloadedStrings #-}

module Spotify.General
    ( isRunning
    , whenRunning
    , withSession
    , maybe'
    ) where


import DBus
import DBus.Client

import Control.Applicative  ((<$>))
import Control.Exception    (catch)


isRunning :: IO Bool
isRunning = withSession $ \client -> flip catch
    (const $ return False :: ClientError -> IO Bool)
    . either (const $ return False) (const $ return True)
        =<< call client (methodCall
            "/org/mpris/MediaPlayer2"
            "org.freedesktop.DBus.Properties"
            "Get")
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.spotify"
        , methodCallBody =
            [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
            , toVariant ("PlaybackStatus"                :: String) ] }


whenRunning :: IO a -> IO (Maybe a)
whenRunning f = do
    running <- isRunning

    if running
    then Just <$> f
    else return Nothing


withSession :: (Client -> IO a) -> IO a
withSession f = do
    client <- connectSession
    result <- f client
    disconnect client
    return result


maybe' :: Maybe a -> c -> (a -> c) -> c
maybe' = flip $ flip . maybe
