module Main (main) where


import Cmus


import Control.Arrow      ((&&&))
import Data.Maybe         (fromJust, fromMaybe)
import System.Environment (getArgs)


main :: IO ()
main = do
    args   <- getArgs
    mquery <- query

    maybe' mquery
        (putStrLn $ case args of
            ["status"]   -> "無"
            ["progress"] -> show 0
            _ -> shit
        )
        $ putStrLn . (case args of
            ["all"]           -> show
            ["status"]        -> showStatus . _status
            ["file"]          -> fromJust . _file
            ["artist", 前]    -> maybeEmpty . prefix 前 . _artist
            ["album", 前]     -> maybeEmpty . prefix 前 . _album
            ["title", 前, n]  -> take (read n) . maybeEmpty . prefix 前 . _title

            ["titles", 前, n] -> \q -> take (read n) . fromMaybe
                (maybeEmpty . prefix 前 . _title $ q)
                . prefix 前 . _stream $ q

            ["progress"]      -> \q -> show $
                if _status q == Stopped
                then 0
                else uncurry div
                   . ((* 100) . fromJust . _position &&&
                      fromDuration . fromJust . _duration)
                   $ q

            _ -> shit)

    where
    showStatus status = case status of
        Playing -> "再"
        Paused  -> "休"
        Stopped -> "止"

    prefix 前 = fmap (前 ++)

    maybeEmpty = fromMaybe ""

    shit = error "悪"

    maybe' = flip $ flip . maybe
