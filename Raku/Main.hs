module Main (main) where


import qualified Cmus    as C
import qualified Spotify as S
import qualified Vlc     as V


import Control.Applicative  ((<$>))
import Control.Monad        (filterM)
import System.Environment   (getArgs)


main :: IO ()
main = do
    args <- getArgs

    case args of
        ("q" : rest)  -> mapM_ (($ rest) . _handleQuery)   =<< running
        ("c" : rest)  -> mapM_ (($ rest) . _handleControl) =<< running
        _             -> putStrLn "悪"

    where
    running = take 1 <$> filterM (fmap (== True) . _isRunning) players
    players = [cmus, spotify, vlc]


cmus, spotify, vlc :: MediaPlayer
cmus = MediaPlayer
    { _isRunning      = C.isRunning
    , _handleQuery    = C.handleQuery
    , _handleControl  = C.handleControl
    }


spotify = MediaPlayer
    { _isRunning      = S.isRunning
    , _handleQuery    = S.handleQuery
    , _handleControl  = S.handleControl
    }


vlc = MediaPlayer
    { _isRunning      = V.isRunning
    , _handleQuery    = V.handleQuery
    , _handleControl  = V.handleControl
    }


data MediaPlayer = MediaPlayer
    { _isRunning      :: IO Bool
    , _handleQuery    :: [String] -> IO ()
    , _handleControl  :: [String] -> IO ()
    }
