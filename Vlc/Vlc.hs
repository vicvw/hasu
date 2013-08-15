{-# LANGUAGE OverloadedStrings #-}

module Vlc
    ( isRunning
    , handleQuery
    , handleControl

    , query
    , play
    , toggle
    , stop
    , previous
    , next

    , Metadata (..)
    , Status (..)
    ) where


import DBus
import DBus.Client

import Control.Applicative  ((<$>))
import Control.Arrow        ((***), (>>>))
import Control.Monad        (void, when)
import Control.Exception    (catch)
import Data.Maybe           (fromJust, fromMaybe)


handleQuery :: [String] -> IO ()
handleQuery args = (putStrLn .) >>> (=<< fromJust <$> query) $ case args of
    ["all"]       -> show
    ["status"]    -> showStatus . _status
    ["url"]       -> _url
    ["title", 前] -> maybeEmpty . prefix 前 . _title
    ["now", 前]   -> maybeEmpty . prefix 前 . _nowPlaying
    ["progress"]  -> const "0"
    _             -> const "悪"

    where
    showStatus status_ = case status_ of
        Playing -> "再"
        Paused  -> "休"
        Stopped -> "止"

    prefix 前  = fmap (前 ++)
    maybeEmpty = fromMaybe ""


handleControl :: [String] -> IO ()
handleControl args = case args of
    ["play"]   -> play
    ["toggle"] -> toggle
    ["stop"]   -> stop
    ["prev"]   -> previous
    ["next"]   -> next
    _          -> error "illegal command"


play, toggle, stop, previous, next :: IO ()
play      = whenRunning "Play"
toggle    = whenRunning "PlayPause"
stop      = whenRunning "Stop"
previous  = whenRunning "Previous"
next      = whenRunning "Next"


whenRunning :: MemberName -> IO ()
whenRunning cmd = do
    running <- isRunning
    when running . void $ control cmd


control :: MemberName -> IO ()
control cmd = withSession $ \client ->
    callNoReply client
        (methodCall
            "/org/mpris/MediaPlayer2"
            "org.mpris.MediaPlayer2.Player"
            cmd)
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.vlc" }


isRunning :: IO Bool
isRunning = withSession $ \client -> flip catch
    (const $ return False :: ClientError -> IO Bool)
    . const (return True) $ call_ client
        (methodCall
            "/org/mpris/MediaPlayer2"
            "org.freedesktop.DBus.Properties"
            "Get")
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.vlc"
        , methodCallBody =
            [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
            , toVariant ("PlaybackStatus"                :: String) ] }


query :: IO (Maybe Metadata)
query = withSession $ \client -> do
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

    status_ <- status
    let (url, title, nowPlaying) = select reply

    return . Just $ Metadata
        { _status     = status_
        , _url        = url
        , _title      = title
        , _nowPlaying = nowPlaying
        }

    where
    select reply
        = (\d -> ( fromJust $ lookup "xesam:url" d
                 , lookup "xesam:title" d
                 , lookup "vlc:nowplaying" d))
        . map ((fromVariant' :: Variant -> String) ***
                fromVariant' . fromVariant')
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


withSession :: (Client -> IO a) -> IO a
withSession f = do
    client <- connectSession
    result <- f client
    disconnect client
    return result


fromVariant' :: IsVariant a => Variant -> a
fromVariant' x = fromJust . fromVariant $ x


data Status
    = Playing
    | Paused
    | Stopped
    deriving (Show)


data Metadata = Metadata
    { _status     :: Status
    , _url        :: String
    , _title      :: Maybe String
    , _nowPlaying :: Maybe String
    } deriving (Show)
