module Main (main) where


import qualified Cmus


import Control.Applicative  ((<$>))
import Control.Monad        (filterM)
import Data.Maybe           (listToMaybe)
import System.Environment   (getArgs)


main :: IO ()
main = do
    arg <- listToMaybe <$> getArgs
    let cmd = maybe (error "no argument") dispatch arg

    mapM_ cmd =<< running

    where
    running = filterM (fmap (== True) . _isRunning) players
    players = [cmus, spotify]


dispatch :: String -> MediaPlayer -> IO ()
dispatch cmd = case cmd of
    "play"   -> _play
    "toggle" -> _toggle
    "stop"   -> _stop
    "prev"   -> _previous
    "next"   -> _next
    _        -> error "illegal command"


cmus :: MediaPlayer
cmus = MediaPlayer
    { _isRunning = Cmus.isRunning
    , _play      = Cmus.play
    , _toggle    = Cmus.toggle
    , _stop      = Cmus.stop
    , _previous  = Cmus.previous
    , _next      = Cmus.next
    }


spotify :: MediaPlayer
spotify = MediaPlayer
    { _isRunning = return False
    , _play      = return ()
    , _toggle    = return ()
    , _stop      = return ()
    , _previous  = return ()
    , _next      = return ()
    }


data MediaPlayer = MediaPlayer
    { _isRunning :: IO Bool
    , _play      :: IO ()
    , _toggle    :: IO ()
    , _stop      :: IO ()
    , _previous  :: IO ()
    , _next      :: IO ()
    }
