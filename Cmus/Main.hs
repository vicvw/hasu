module Main (main) where


import Cmus


import Control.Arrow      ((&&&))
import Data.Maybe         (fromJust, fromMaybe)
import System.Environment (getArgs)


main :: IO ()
main = do
    cmd:_  <- getArgs
    mquery <- cmusQuery

    maybe' mquery
        (putStrLn $ case cmd of
            "status"   -> "無"
            "progress" -> show 0
            _          -> noArgument
        )
        $ putStrLn . (case cmd of
            "all"      -> show
            "status"   -> showStatus . _status
            "file"     -> fromJust . _file
            "artist"   -> fromMaybe "" . _artist
            "artistc"  -> fromMaybe "" . fmap ("    " ++) . _artist
            "album"    -> fromMaybe "" . _album
            "title"    -> fromMaybe "" . _title
            "titlec"   -> fromMaybe "" . fmap ("    " ++) . _title
            "progress" -> \query -> show $
                if _status query == Stopped
                then 0
                else uncurry div
                   . ((* 100) . fromJust . _position &&&
                      fromDuration . fromJust . _duration)
                   $ query
            _          -> noArgument)

    where
    showStatus status = case status of
        Playing -> "再"
        Paused  -> "休"
        Stopped -> "止"

    noArgument = error "no argument"

    maybe' = flip $ flip . maybe