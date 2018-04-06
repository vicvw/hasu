{-# LANGUAGE OverloadedStrings #-}

module Spotify.General
    ( isRunning
    , whenRunning
    , withSession
    , fromVariant'
    ) where


import Control.Applicative  ((<$>))
import Control.Exception    (handle)
import Data.Maybe           (fromJust)
import DBus
import DBus.Client


isRunning :: IO Bool
isRunning = withSession $ \client -> handle
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


fromVariant' :: IsVariant a => Variant -> a
fromVariant' x = fromJust . fromVariant $ x
