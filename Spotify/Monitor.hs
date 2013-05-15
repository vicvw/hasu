module Monitor
    ( getCurrentTime
    , listenForAd
    ) where


import Spotify

import Control.Concurrent (threadDelay)
import Data.Time.Clock    (diffUTCTime, getCurrentTime, UTCTime)
import System.Cmd         (system)


listenForAd :: Metadata -> UTCTime -> IO ()
listenForAd oldMeta a = do
    threadDelay $ 2 * 10^6
    system "clear"

    meta <- getMetadata
    diff <- getTimeDiff a

    let diffB = diff >= m_length meta
        metaB = meta == oldMeta
        titleD = (m_title oldMeta, m_title meta)

    print diff
    print $ m_length meta
    print diffB
    print metaB
    print titleD

    if  diffB && metaB
      then do
        putStrLn "ad"
        -- Pulse.mute "spotify"

        if metaB
          then
            listenForAd oldMeta a
          else do
            a' <- getCurrentTime
            listenForAd oldMeta a'
      else do
        putStrLn "no ad"
        -- Pulse.unmute "spotify"
        listenForAd meta a


getTimeDiff :: UTCTime -> IO Integer
getTimeDiff a = do
    b <- getCurrentTime

    let μ = truncate
          . (10^6 *)
          . toRational
          $ diffUTCTime b a

    return μ
