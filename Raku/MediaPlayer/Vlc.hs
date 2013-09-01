module MediaPlayer.Vlc
    ( vlc
    ) where


import MediaPlayer.Interface

import qualified Vlc as V


import Control.Applicative  ((<$>), (<|>))
import Control.Arrow        ((&&&))
import Data.Maybe           (fromJust, fromMaybe)

import System.FilePath      (takeFileName)


vlc :: MediaPlayer
vlc = MediaPlayer
    { _isRunning  = V.isRunning

    , _status     = showStatus . V._status <$> query
    , _progress   = show
                  . uncurry div
                  . ((* 100) . V._position
                     &&&
                     fromMaybe (10^6) . V._length)
                <$> query

    , _artist     = const $ return ""
    , _album      = const $ return ""
    , _title      = \p -> maybeEmpty
                        . prefix p
                        . foldr1 (<|>)
                        . zipWith ($) [V._nowPlaying, V._title, Just . takeFileName . V._url]
                        . repeat
                      <$> query

    , _play       = V.play
    , _toggle     = V.toggle
    , _stop       = V.stop
    , _previous   = V.previous
    , _next       = V.next
    }

    where
    query = fromJust <$> V.query

    showStatus status = case status of
        V.Playing -> "再"
        V.Paused  -> "休"
        V.Stopped -> "止"

    prefix 前 = fmap (前 ++)
    maybeEmpty = fromMaybe ""
