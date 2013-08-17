{-# LANGUAGE OverloadedStrings #-}

module Spotify.Control
    ( play
    , toggle
    , stop
    , previous
    , next
    ) where


import Spotify.General


import DBus
import DBus.Client

import Control.Monad  (void)


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
