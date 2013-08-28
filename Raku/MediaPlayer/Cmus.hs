module MediaPlayer.Cmus
    ( cmus
    ) where


import MediaPlayer.Interface

import qualified Cmus as C


import Control.Applicative  ((<$>), (<|>))
import Control.Arrow        ((&&&))
import Data.Maybe           (fromJust, fromMaybe)


cmus :: MediaPlayer
cmus = MediaPlayer
    { _isRunning  = C.isRunning

    , _status     = showStatus . C._status <$> query
    , _progress   = (<$> query) $ \q -> show $
                        if C._status q == C.Stopped
                        then 0
                        else ($ q)
                            $ uncurry div
                            . ((* 100) . fromJust . C._position
                               &&&
                               C.fromDuration . fromJust . C._duration)

    , _artist     = \p -> maybeEmpty . prefix p . C._artist <$> query
    , _album      = \p -> maybeEmpty . prefix p . C._album  <$> query
    , _title      = \p -> maybeEmpty
                        . prefix p
                        . foldr1 (<|>)
                        . zipWith ($) [C._stream, C._title, C._file]
                        . repeat
                      <$> query

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

    prefix 前 = fmap (前 ++)
    maybeEmpty = fromMaybe ""
