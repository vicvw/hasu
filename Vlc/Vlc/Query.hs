{-# LANGUAGE OverloadedStrings #-}

module Vlc.Query
    ( handleQuery
    , query
    , Metadata (..)
    , Status (..)
    ) where


import Vlc.General


import DBus
import DBus.Client

import Control.Applicative  ((<$>), (<|>))
import Control.Arrow        ((&&&), (***), (>>>))

import Data.Maybe           (fromJust, fromMaybe)
import Data.Int             (Int64)

import System.FilePath      (takeFileName)


handleQuery :: [String] -> IO ()
handleQuery args = (putStrLn .) >>> (=<< fromJust <$> query) $ case args of
    ["all"]         -> show
    ["status"]      -> showStatus . _status
    ["url"]         -> _url
    ["artist", _]   -> const ""

    ["title", 前]   -> (前 ++)
                     . uncurry fromMaybe
                     . (take 40 . takeFileName . _url
                        &&&
                        uncurry (<|>) . (_title &&& _nowPlaying))

    ["progress"]    -> show
                     . uncurry div
                     . ((* 100) . _position
                        &&&
                        fromMaybe (10^6) . _length)

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
            "/org/mpris/MediaPlayer2"
            "org.freedesktop.DBus.Properties"
            "Get")
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.vlc"
        , methodCallBody =
            [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
            , toVariant ("Metadata"                      :: String) ] }

    replyPosition <- call_ client
        (methodCall
            "/org/mpris/MediaPlayer2"
            "org.freedesktop.DBus.Properties"
            "Get")
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.vlc"
        , methodCallBody =
            [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
            , toVariant ("Position"                      :: String) ] }

    status_ <- status
    let dict = select reply
        pos  = fromVariant'
             . fromVariant'
             . head
             . methodReturnBody
             $ replyPosition

    return $ Metadata
        { _status     = status_
        , _url        = fromVariant' . fromJust $ lookup "xesam:url" dict
        , _title      = fromVariant' <$> lookup "xesam:title" dict
        , _nowPlaying = fromVariant' <$> lookup "vlc:nowplaying" dict
        , _length     = fromIntegral <$> (fromVariant' <$> lookup "mpris:length" dict :: Maybe Int64)
        , _position   = fromIntegral (pos :: Int64)
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
                    Just "org.mpris.MediaPlayer2.vlc"
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
    , _url        :: String
    , _title      :: Maybe String
    , _nowPlaying :: Maybe String
    , _length     :: Maybe Integer
    , _position   :: Integer
    } deriving (Show)


data Status
    = Playing
    | Paused
    | Stopped
    deriving (Show)
