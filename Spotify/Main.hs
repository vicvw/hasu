{-# LANGUAGE OverloadedStrings #-}

module Main where


import DBus
import DBus.Client

import Data.List  (sort)


main = do
    client  <-  connectSession
    reply   <-  call_ client (methodCall "/org/mpris/MediaPlayer2"
                                         "org.freedesktop.DBus.Properties"
                                         "Get")
                    { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
                    , methodCallBody        = [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
                                              , toVariant ("Metadata" :: String) ]
                    }

    let Just names = fromVariant . head $ methodReturnBody reply

    mapM_ putStrLn $ sort names
