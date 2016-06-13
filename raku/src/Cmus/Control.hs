module Cmus.Control
    ( play
    , toggle
    , stop
    , previous
    , next
    ) where


import Cmus.General
import Cmus.Query     (query, _status, _stream, Status (..))


import Control.Monad  (void, when)


play, toggle, stop, previous, next :: IO ()
play     = whenRunning Play
toggle   = toggleStream
stop     = whenRunning Stop
previous = whenRunning Previous
next     = whenRunning Next


toggleStream :: IO ()
toggleStream = maybe
    (return ())
    (\q -> maybe
        (void $ cmus Toggle)
        (const $ case _status q of
            Playing -> void $ cmus Stop
            Paused  -> return ()
            Stopped -> void $ cmus Play)
        $ _stream q)
    =<< query


whenRunning :: Command -> IO ()
whenRunning = (isRunning >>=) . flip when . void . cmus
