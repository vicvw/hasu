module MediaPlayer.Interface
    ( MediaPlayer (..)
    ) where


data MediaPlayer = MediaPlayer
    { _isRunning      :: IO Bool

    , _status         :: IO String
    , _progress       :: IO String
    , _artist         :: String -> IO String
    , _album          :: String -> IO String
    , _title          :: String -> IO String

    , _play           :: IO ()
    , _toggle         :: IO ()
    , _stop           :: IO ()
    , _previous       :: IO ()
    , _next           :: IO ()
    }
