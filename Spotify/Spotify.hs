{-# LANGUAGE OverloadedStrings #-}

module Spotify
    ( getMetadata
    , Metadata (..)
    ) where


import DBus
import DBus.Client    (call_, connectSession)

import Control.Arrow  ((***))

import Data.List      (sort)
import Data.Maybe     (fromJust)

import Data.Int       (Int32)
import Data.Word      (Word64)


getMetadata :: IO Metadata
getMetadata = do
    client <- connectSession
    reply  <- call_ client (methodCall "/org/mpris/MediaPlayer2"
                                       "org.freedesktop.DBus.Properties"
                                       "Get")
                  { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
                  , methodCallBody        = [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
                                            , toVariant ("Metadata" :: String) ] }

    let meta :: [(String, Variant)]
        meta = map ((fromJust . fromVariant) ***
                    (fromJust . fromVariant))
             . dictionaryItems
             . fromJust
             . fromVariant
             . fromJust
             . fromVariant
             . head
             . methodReturnBody
             $ reply

    return Metadata
        { m_artUrl         = lookup' "mpris:artUrl"                     meta :: String
        , m_length         = fromIntegral (lookup' "mpris:length"       meta :: Word64)
        , m_trackID        = lookup' "mpris:trackid"                    meta :: String
        , m_album          = lookup' "xesam:album"                      meta :: String
        , m_artist         = lookup' "xesam:artist"                     meta :: [String]
        , m_autoRating     = lookup' "xesam:autoRating"                 meta :: Double
        , m_contentCreated = lookup' "xesam:contentCreated"             meta :: String
        , m_discNumber     = fromIntegral (lookup' "xesam:discNumber"   meta :: Int32)
        , m_title          = lookup' "xesam:title"                      meta :: String
        , m_trackNumber    = fromIntegral (lookup' "xesam:trackNumber"  meta :: Int32)
        , m_url            = lookup' "xesam:url"                        meta :: String
        }

    where
    lookup' :: IsVariant a => String -> [(String, Variant)] -> a
    lookup' string meta
        = fromJust
        . fromVariant
        . fromJust
        . lookup string
        $ meta


data Metadata = Metadata
    { m_album           :: String
    , m_artist          :: [String]
    , m_artUrl          :: String
    , m_autoRating      :: Double
    , m_contentCreated  :: String
    , m_discNumber      :: Integer
    , m_length          :: Integer
    , m_title           :: String
    , m_trackID         :: String
    , m_trackNumber     :: Integer
    , m_url             :: String
    } deriving Show
