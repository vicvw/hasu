module MediaPlayer.Cmus
    ( cmus
    ) where


import Control.Applicative  ((<$>), (<|>))
import Control.Arrow        ((&&&), (***))
import Control.Monad        (join)
import Data.Maybe           (fromJust, fromMaybe)


import qualified Cmus as C

import MediaPlayer.Interface


cmus :: MediaPlayer
cmus = MediaPlayer
    { _isRunning  = C.isRunning

    , _status     = showStatus . C._status <$> query
    , _progress   = (<$> query) $ \q -> show $
                        if C._status q == C.Stopped
                        then 0
                        else ($ q)
                            $ round . uncurry (/)
                            . join (***) fromIntegral
                            . ((* 100) . fromJust . C._position
                               &&&
                               C.fromDuration . fromJust . C._duration)

    , _artist     = maybeEmpty . C._artist <$> query
    , _album      = maybeEmpty . C._album  <$> query
    , _title      = (<$> query)
                  $ maybeEmpty
                  . foldr1 (<|>)
                  . zipWith ($) [C._stream, C._title, C._file]
                  . repeat

    , _play       = C.play
    , _toggle     = C.toggle
    , _stop       = C.stop
    , _previous   = C.previous
    , _next       = C.next
    }

    where
    query = fromJust <$> C.query

    showStatus status = case status of
        C.Playing -> "再"
        C.Paused  -> "休"
        C.Stopped -> "止"

    maybeEmpty = fromMaybe ""
