module Cmus.Control
    ( handleControl
    , play
    , toggle
    , stop
    , previous
    , next
    ) where


import Cmus.General


import Control.Monad  (void, when)


handleControl :: [String] -> IO ()
handleControl args = case args of
    ["play"]   -> play
    ["toggle"] -> toggle
    ["stop"]   -> stop
    ["prev"]   -> previous
    ["next"]   -> next
    _          -> error "悪"


play, toggle, stop, previous, next :: IO ()
play     = whenRunning Play
toggle   = whenRunning Toggle
stop     = whenRunning Stop
previous = whenRunning Previous
next     = whenRunning Next


whenRunning :: Command -> IO ()
whenRunning = (isRunning >>=) . flip when . void . cmus
