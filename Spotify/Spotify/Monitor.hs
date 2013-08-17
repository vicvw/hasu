module Spotify.Monitor
    ( muteAds
    ) where


import Spotify.General
import Spotify.Query

import Volume


import Control.Applicative  ((<$>))
import Control.Concurrent   (threadDelay)
import Control.Monad        (join, unless, void)
import Data.Function        (fix)
import Data.List            (find, isInfixOf, isPrefixOf)
import System.Posix.Process (forkProcess)
import System.Process       (readProcess, system)


muteAds :: IO ()
muteAds = flip fix False $ \loop wasAd -> do
    threadDelay $ 10^6

    mtitle <- fmap _title <$> query

    maybe' mtitle (return ()) $ \title -> do
        micon <- wmIconName

        maybe' micon (loop wasAd) $ \icon -> do
            -- print title
            -- print icon

            if isAd title icon
            then do
                print True
                muteApp name

                unless wasAd .
                    void . forkProcess . void $ system "notify-send '広告'"

                loop True

            else do
                print False
                unmuteApp name

                if wasAd
                then do
                    threadDelay . floor $ 1.5 * 10^6
                    loop False
                else
                    loop wasAd

    where
    isAd = (not .) . isInfixOf
    name = "spotify"


wmIconName :: IO (Maybe String)
wmIconName = fmap join . whenRunning $
    (<$> readProcess "xprop" ["-name", "Spotify - Linux Preview"] "")
    $ find (isPrefixOf "WM_ICON_NAME(COMPOUND_TEXT)") . lines













-- import Control.Concurrent (threadDelay)
-- import Data.Time.Clock    (diffUTCTime, getCurrentTime, UTCTime)
-- import System.Cmd         (system)


-- listenForAd :: Metadata -> UTCTime -> IO ()
-- listenForAd oldMeta a = do
--     threadDelay $ 2 * 10^6
--     system "clear"

--     meta <- query
--     diff <- getTimeDiff a

--     let diffB = diff >= _length meta
--         metaB = meta == oldMeta
--         titleD = (_title oldMeta, _title meta)

--     print diff
--     print $ _length meta
--     print diffB
--     print metaB
--     print titleD

--     if  diffB && metaB
--       then do
--         putStrLn "ad"
--         -- Pulse.mute "spotify"

--         if metaB
--           then
--             listenForAd oldMeta a
--           else do
--             a' <- getCurrentTime
--             listenForAd oldMeta a'
--       else do
--         putStrLn "no ad"
--         -- Pulse.unmute "spotify"
--         listenForAd meta a


-- getTimeDiff :: UTCTime -> IO Integer
-- getTimeDiff a = do
--     b <- getCurrentTime

--     let μ = truncate
--           . (10^6 *)
--           . toRational
--           $ diffUTCTime b a

--     return μ
