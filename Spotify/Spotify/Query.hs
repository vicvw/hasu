{-# LANGUAGE OverloadedStrings #-}

module Spotify.Query
    ( handleQuery
    , query
    , status
    , Metadata (..)
    ) where


import Spotify.General


import DBus
import DBus.Client

import Control.Arrow        ((&&&), (***), (>>>))
import Control.Applicative  ((<$>))

import Data.Maybe           (fromJust)

import Data.Int             (Int32, Int64)
import Data.Word            (Word64)


handleQuery :: [String] -> IO ()
handleQuery args = (putStrLn .) >>> (=<< fromJust <$> query) $ case args of
    ["all"]         -> show
    ["status"]      -> showStatus . _status
    ["url"]         -> _url
    ["artist", 前]  -> (前 ++) . head . _artist
    ["title", 前]   -> (前 ++) . _title
    ["now", 前]     -> (前 ++) . _title
    ["progress"]    -> show . uncurry div . (_position &&& _length)
    _               -> const "悪"

    where
    showStatus status_ = case status_ of
        Playing -> "再"
        Paused  -> "休"
        Stopped -> "止"


query :: IO (Maybe Metadata)
query = whenRunning . withSession $ \client -> do
    reply <- call_ client
        (methodCall
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

    status_   <- fromJust <$> status
    position_ <- fromJust <$> position

    return Metadata
        { _status         = status_
        , _artist         = lookup' "xesam:artist"                     meta :: [String]
        , _album          = lookup' "xesam:album"                      meta :: String
        , _discNumber     = fromIntegral (lookup' "xesam:discNumber"   meta :: Int32)
        , _title          = lookup' "xesam:title"                      meta :: String
        , _trackNumber    = fromIntegral (lookup' "xesam:trackNumber"  meta :: Int32)
        , _length         = fromIntegral (lookup' "mpris:length"       meta :: Word64)
        , _position       = position_
        , _url            = lookup' "xesam:url"                        meta :: String
        , _trackID        = lookup' "mpris:trackid"                    meta :: String
        , _artUrl         = lookup' "mpris:artUrl"                     meta :: String
        , _autoRating     = lookup' "xesam:autoRating"                 meta :: Double
        , _contentCreated = lookup' "xesam:contentCreated"             meta :: String
        }

    where
    lookup' :: IsVariant a => String -> [(String, Variant)] -> a
    lookup' string meta
        = fromVariant'
        . fromJust
        . lookup string
        $ meta


status :: IO (Maybe Status)
status = whenRunning . withSession $ \client -> do
    reply <- call_ client
        (methodCall
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
        _         -> undefined


position :: IO (Maybe Integer)
position = whenRunning . withSession $ \client -> do
    reply <- call_ client
        (methodCall
            "/org/mpris/MediaPlayer2"
            "org.freedesktop.DBus.Properties"
            "Get")
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.spotify"
        , methodCallBody =
            [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
            , toVariant ("Position"                      :: String) ] }

    let position_ :: Int64
        position_ = fromVariant'
                  . fromVariant'
                  . head
                  . methodReturnBody
                  $ reply

    return $ fromIntegral position_


fromVariant' :: IsVariant a => Variant -> a
fromVariant' x = fromJust $ fromVariant x


data Metadata = Metadata
    { _status         :: Status
    , _artist         :: [String]
    , _album          :: String
    , _discNumber     :: Integer
    , _title          :: String
    , _trackNumber    :: Integer
    , _length         :: Integer
    , _position       :: Integer
    , _url            :: String
    , _trackID        :: String
    , _artUrl         :: String
    , _autoRating     :: Double
    , _contentCreated :: String
    } deriving (Show)


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
