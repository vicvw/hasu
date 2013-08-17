{-# LANGUAGE OverloadedStrings #-}

module Spotify.Query
    ( query
    , status
    , Metadata (..)
    ) where


import Spotify.General


import DBus
import DBus.Client    (call_, connectSession)

import Control.Arrow  ((***))

import Data.Maybe     (fromJust)

import Data.Int       (Int32)
import Data.Word      (Word64)


query :: IO (Maybe Metadata)
query = whenRunning . withSession $ \client -> do
    reply  <- call_ client (methodCall
                "/"
                "org.freedesktop.MediaPlayer2"
                "GetMetadata")
                { methodCallDestination =
                    Just "com.spotify.qt" }

    let meta :: [(String, Variant)]
        meta = map (fromVariant' *** fromVariant')
             . dictionaryItems
             . fromVariant'
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
        = fromVariant'
        . fromJust
        . lookup string
        $ meta


status :: IO Status
status = do
    client <- connectSession
    reply  <- call_ client (methodCall "/org/mpris/MediaPlayer2"
                                       "org.freedesktop.DBus.Properties"
                                       "Get")
                  { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
                  , methodCallBody        = [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
                                            , toVariant ("Status" :: String)
                                            ] }

    let status_ :: String
        status_ = fromVariant'
                . fromVariant'
                . head
                . methodReturnBody
                $ reply

    return $ case status_ of
        "Playing" -> Playing
        "Paused"  -> Paused
        "Stopped" -> Stopped
        _         -> undefined


fromVariant' :: IsVariant a => Variant -> a
fromVariant' x = fromJust $ fromVariant x


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


data Status
    = Playing
    | Paused
    | Stopped
    deriving (Show)


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
