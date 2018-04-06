{-# LANGUAGE OverloadedStrings #-}

module Spotify.Query
    ( query
    , Metadata (..)
    , Status (..)
    ) where


import Control.Applicative  ((<$>))
import Control.Arrow        ((***))
import Data.Int             (Int64)
import Data.Maybe           (fromJust)
import DBus
import DBus.Client


import Spotify.General


query :: IO (Maybe Metadata)
query = whenRunning . withSession $ \client -> do
    reply <- call_ client
        (methodCall
            "/org/mpris/MediaPlayer2"
            "org.freedesktop.DBus.Properties"
            "Get")
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.spotify"
        , methodCallBody =
            [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
            , toVariant ("Metadata"                      :: String) ] }

    status_ <- status
    let dict = select reply

    return Metadata
        { _status     = status_
        , _artist     = fromVariant' (fromJust $ lookup "xesam:albumArtist" dict)
        , _title      = fromVariant' (fromJust $ lookup "xesam:title" dict)
        , _length     = fromIntegral (fromJust $ fromVariant' <$> lookup "mpris:length" dict :: Int64)
        , _position   = 0
        }

    where
    select reply
        = map ((fromVariant' :: Variant -> String) ***
                fromVariant')
        . dictionaryItems
        . fromVariant'
        . fromVariant'
        . head
        . methodReturnBody
        $ reply


status :: IO Status
status = withSession $ \client -> do
    reply <- call_ client (methodCall
                    "/org/mpris/MediaPlayer2"
                    "org.freedesktop.DBus.Properties"
                    "Get")
                { methodCallDestination =
                    Just "org.mpris.MediaPlayer2.spotify"
                , methodCallBody =
                    [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
                    , toVariant ("PlaybackStatus"                :: String) ] }

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


data Metadata = Metadata
    { _status     :: Status
    , _artist     :: [String]
    , _title      :: String
    , _length     :: Integer
    , _position   :: Integer
    } deriving (Show)


data Status
    = Playing
    | Paused
    | Stopped
    deriving (Show, Eq)
