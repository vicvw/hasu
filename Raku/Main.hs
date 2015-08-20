module Main (main) where


import MediaPlayer.Interface
import MediaPlayer.Cmus
import MediaPlayer.Vlc

import Nara


import Control.Applicative  ((<$>))
import Control.Monad        ((>=>), filterM)
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
                ["status"]    -> _status   >=> putStrLn
                ["progress"]  -> _progress >=> putStrLn
                ["artist", p] -> _artist   >=> putStrLn . postfix p
                ["album",  p] -> _album    >=> putStrLn . postfix p
                ["title",  p] -> _title    >=> putStrLn . postfix p . elide
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
    postfix p = if' null id (++ p)
    elide     = if' ((> n) . length) ((++ "…") . take n) id where n = 70
    fail      = putStrLn . const "失"

    running   = listToMaybe . take 1 <$> filterM (fmap (== True) . _isRunning) players
    players   = [vlc, cmus]
