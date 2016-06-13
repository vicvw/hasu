module MediaPlayer.Spotify
    ( spotify
    ) where


import MediaPlayer.Interface

import qualified Spotify as S


import Control.Applicative  ((<$>))
import Control.Arrow        ((&&&))
import Data.List            (intercalate)
import Data.Maybe           (fromJust)


spotify :: MediaPlayer
spotify = MediaPlayer
    { _isRunning  = S.isRunning

    , _status     = showStatus . S._status <$> query
    , _progress   = show
                  . uncurry div
                  . ((* 100) . S._position
                     &&&
                     S._length)
                <$> query

    , _artist     = \p -> (p ++)
                        . intercalate ", "
                        . S._artist
                      <$> query
    , _album      = const $ return ""
    , _title      = \p -> (p ++)
                        . S._title
                      <$> query

    , _play       = S.play
    , _toggle     = S.toggle
    , _stop       = S.stop
    , _previous   = S.previous
    , _next       = S.next
    }

    where
    query = fromJust <$> S.query

    showStatus status = case status of
        S.Playing -> "再"
        S.Paused  -> "休"
        S.Stopped -> "止"
