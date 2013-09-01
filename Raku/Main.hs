module Main (main) where


-- import qualified Spotify as S

import MediaPlayer.Interface
import MediaPlayer.Cmus
import MediaPlayer.Vlc


import Control.Applicative  ((<$>))
-- import Control.Arrow        ((>>>))
import Control.Monad        ((<=<), filterM)
import Data.Maybe
import System.Environment   (getArgs)


main :: IO ()
main = do
    args <- getArgs

    (running >>=) $ maybe
        (case args of
            ["q", "status"]   -> putStrLn "無"
            ["q", "progress"] -> putStrLn "0"
            _                 -> putStrLn "")

        (case args of
            ("q" : cmd) -> case cmd of
                ["status"]    -> putStrLn <=< _status
                ["progress"]  -> putStrLn <=< _progress
                ["artist", p] -> putStrLn <=< ($ p) . _artist
                ["album",  p] -> putStrLn <=< ($ p) . _album
                ["title",  p] -> putStrLn <=< ($ p) . _title

            ("c" : cmd) -> case cmd of
                ["play"]      -> _play
                ["toggle"]    -> _toggle
                ["stop"]      -> _stop
                ["prev"]      -> _previous
                ["next"]      -> _next

            _ -> const $ putStrLn "悪")

    where
    running = listToMaybe . take 1 <$> filterM (fmap (== True) . _isRunning) players
    -- players = [vlc, spotify, cmus]
    players = [vlc, cmus]


-- spotify = MediaPlayer
--     { _isRunning      = S.isRunning
--     , _handleQuery    = S.handleQuery
--     , _handleControl  = S.handleControl
--     }


-- vlc = MediaPlayer
--     { _isRunning      = V.isRunning
--     , _handleQuery    = V.handleQuery
--     , _handleControl  = V.handleControl
--     }
