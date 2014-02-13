module Main (main) where


import MediaPlayer.Interface
import MediaPlayer.Cmus
import MediaPlayer.Spotify
import MediaPlayer.Vlc


import Control.Applicative  ((<$>))
import Control.Monad        ((<=<), filterM)
import Data.Maybe           (listToMaybe)
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
                _             -> const $ putStrLn "悪"

            ("c" : cmd) -> case cmd of
                ["play"]      -> _play
                ["toggle"]    -> _toggle
                ["stop"]      -> _stop
                ["prev"]      -> _previous
                ["next"]      -> _next
                _             -> const $ putStrLn "悪"

            _ -> const $ putStrLn "悪")

    where
    running = listToMaybe . take 1 <$> filterM (fmap (== True) . _isRunning) players
    players = [vlc, spotify, cmus]
