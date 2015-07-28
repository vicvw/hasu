module Main (main) where


import MediaPlayer.Interface
import MediaPlayer.Cmus
import MediaPlayer.Vlc

import Nara


import Control.Applicative  ((<$>))
import Control.Monad        ((<=<), filterM)
import Data.Maybe           (listToMaybe)
import System.Environment   (getArgs)


main :: IO ()
main = do
    args <- getArgs

    (running >>=) $ maybe
        (putStrLn $ case args of
            ["q", "status"]   -> "無"
            ["q", "progress"] -> "0"
            _                 -> "")

        (case args of
            "q" : cmd -> case cmd of
                ["status"]    -> putStrLn         <=< _status
                ["progress"]  -> putStrLn         <=< _progress
                ["artist", p] -> putStrLn         <=< postfix p . _artist
                ["album",  p] -> putStrLn         <=< postfix p . _album
                ["title",  p] -> putStrLn . elide <=< postfix p . _title
                _             -> fail

            "c" : cmd -> case cmd of
                ["play"]      -> _play
                ["toggle"]    -> _toggle
                ["stop"]      -> _stop
                ["prev"]      -> _previous
                ["next"]      -> _next
                _             -> fail

            _ -> fail)

    where
    postfix = fmap . flip (++)
    elide   = if' ((> 70) . length) ((++ "…") . take 70) id
    fail    = putStrLn . const "失"

    running = listToMaybe . take 1 <$> filterM (fmap (== True) . _isRunning) players
    players = [vlc, cmus]
