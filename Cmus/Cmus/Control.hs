module Cmus.Control
    ( play
    , toggle
    , stop
    , previous
    , next
    ) where


import Cmus.General


import Control.Monad  (void, when)


play, toggle, stop, previous, next :: IO ()
play     = whenRunning Play
toggle   = whenRunning Toggle
stop     = whenRunning Stop
previous = whenRunning Previous
next     = whenRunning Next


whenRunning :: Command -> IO ()
whenRunning = (isRunning >>=) . flip when . void . cmus
