{-# LANGUAGE OverloadedStrings #-}

module Spotify
    ( getMetadata
    , getPlaybackStatus
    , Metadata (..)
    ) where


import DBus
import DBus.Client    (call_, connectSession)

import Control.Arrow  ((***))

import Data.Maybe     (fromJust)

import Data.Int       (Int32)
import Data.Word      (Word64)


getMetadata :: IO Metadata
getMetadata = do
    client <- connectSession
    reply  <- call_ client
                    (methodCall "/"
                                "org.freedesktop.MediaPlayer2"
                                "GetMetadata")
                    { methodCallDestination = Just "com.spotify.qt" }

    let meta :: [(String, Variant)]
        meta = map ((fromJust . fromVariant) ***
                    (fromJust . fromVariant))
             . dictionaryItems
             . fromJust
             . fromVariant
             . head
             . methodReturnBody
             $ reply

    return Metadata
        { _artUrl         = lookup' "mpris:artUrl"                     meta :: String
        , _length         = fromIntegral (lookup' "mpris:length"       meta :: Word64)
        , _trackID        = lookup' "mpris:trackid"                    meta :: String
        , _album          = lookup' "xesam:album"                      meta :: String
        , _artist         = lookup' "xesam:artist"                     meta :: [String]
        , _autoRating     = lookup' "xesam:autoRating"                 meta :: Double
        , _contentCreated = lookup' "xesam:contentCreated"             meta :: String
        , _discNumber     = fromIntegral (lookup' "xesam:discNumber"   meta :: Int32)
        , _title          = lookup' "xesam:title"                      meta :: String
        , _trackNumber    = fromIntegral (lookup' "xesam:trackNumber"  meta :: Int32)
        , _url            = lookup' "xesam:url"                        meta :: String
        }

    where
    lookup' :: IsVariant a => String -> [(String, Variant)] -> a
    lookup' string meta
        = fromJust
        . fromVariant
        . fromJust
        . lookup string
        $ meta


getPlaybackStatus :: IO PlaybackStatus
getPlaybackStatus = do
    client <- connectSession
    reply  <- call_ client (methodCall "/org/mpris/MediaPlayer2"
                                       "org.freedesktop.DBus.Properties"
                                       "Get")
                  { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
                  , methodCallBody        = [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
                                            , toVariant ("PlaybackStatus" :: String)
                                            ] }

    let status :: String
        status = fromJust
               . fromVariant
               . fromJust
               . fromVariant
               . head
               . methodReturnBody
               $ reply

    return $ case status of
        "Playing" -> Playing
        "Paused"  -> Paused
        "Stopped" -> Stopped
        _         -> undefined


data PlaybackStatus
    = Playing
    | Paused
    | Stopped


data Metadata = Metadata
    { _album           :: String
    , _artist          :: [String]
    , _artUrl          :: String
    , _autoRating      :: Double
    , _contentCreated  :: String
    , _discNumber      :: Integer
    , _length          :: Integer
    , _title           :: String
    , _trackID         :: String
    , _trackNumber     :: Integer
    , _url             :: String
    } deriving (Show, Eq)


    -- reply  <- call_ client (methodCall "/org/mpris/MediaPlayer2"
    --                                    "org.freedesktop.DBus.Properties"
    --                                    "Get")
    --               { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
    --               , methodCallBody        = [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
    --                                         , toVariant ("Metadata" :: String) ] }

    -- let meta :: [(String, Variant)]
    --     meta = map ((fromJust . fromVariant) ***
    --                 (fromJust . fromVariant))
    --          . dictionaryItems
    --          . fromJust
    --          . fromVariant
    --          . fromJust
    --          . fromVariant
    --          . head
    --          . methodReturnBody
    --          $ reply
