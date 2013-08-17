{-# LANGUAGE OverloadedStrings #-}

module Spotify.Control
    ( handleControl
    , play
    , toggle
    , stop
    , previous
    , next
    ) where


import Spotify.General


import DBus
import DBus.Client

import Control.Monad  (void)


handleControl :: [String] -> IO ()
handleControl args = case args of
    ["play"]   -> play
    ["toggle"] -> toggle
    ["stop"]   -> stop
    ["prev"]   -> previous
    ["next"]   -> next
    _          -> error "悪"


play, toggle, stop, previous, next :: IO ()
play     = control "Play"
toggle   = control "PlayPause"
stop     = control "Stop"
previous = control "Previous"
next     = control "Next"


control :: MemberName -> IO ()
control cmd = void . whenRunning . withSession $ \client ->
    callNoReply client
        (methodCall
            "/org/mpris/MediaPlayer2"
            "org.mpris.MediaPlayer2.Player"
            cmd)
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.spotify" }
