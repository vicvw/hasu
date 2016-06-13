{-# LANGUAGE OverloadedStrings #-}

module Vlc.Control
    ( play
    , toggle
    , stop
    , previous
    , next
    ) where


import Control.Monad  (void)
import DBus
import DBus.Client


import Vlc.General


play, toggle, stop, previous, next :: IO ()
play      = control "Play"
toggle    = control "PlayPause"
stop      = control "Stop"
previous  = control "Previous"
next      = control "Next"


control :: MemberName -> IO ()
control cmd = void . whenRunning . withSession $ \client ->
    callNoReply client
        (methodCall
            "/org/mpris/MediaPlayer2"
            "org.mpris.MediaPlayer2.Player"
            cmd)
        { methodCallDestination =
            Just "org.mpris.MediaPlayer2.vlc" }
