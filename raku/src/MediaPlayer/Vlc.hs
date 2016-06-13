module MediaPlayer.Vlc
    ( vlc
    ) where


import MediaPlayer.Interface

import qualified Vlc as V


import Control.Applicative  ((<$>), (<|>))
import Control.Arrow        ((&&&), (***))
import Control.Monad        (join)
import Data.Maybe           (fromJust, fromMaybe)
import Network.HTTP.Base    (urlDecode)

import System.FilePath      (takeFileName)


vlc :: MediaPlayer
vlc = MediaPlayer
    { _isRunning  = V.isRunning

    , _status     = showStatus . V._status <$> query
    , _progress   = (<$> query)
                  $ show . round . uncurry (/)
                  . join (***) fromIntegral
                  . ((* 100) . V._position
                     &&&
                     fromMaybe (10^6) . V._length)

    , _artist     = return ""
    , _album      = return ""
    , _title      = (<$> query)
                  $ urlDecode
                  . fromMaybe ""
                  . foldr1 (<|>)
                  . zipWith ($) [V._nowPlaying, V._title, Just . takeFileName . V._url]
                  . repeat

    , _play       = V.play
    , _toggle     = V.toggle
    , _stop       = V.stop
    , _previous   = V.previous
    , _next       = V.next
    }

    where
    query = fromJust <$> V.query

    showStatus V.Playing = "再"
    showStatus V.Paused  = "休"
    showStatus V.Stopped = "止"
