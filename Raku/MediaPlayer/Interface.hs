module MediaPlayer.Interface
    ( MediaPlayer (..)
    ) where


data MediaPlayer = MediaPlayer
    { _isRunning      :: IO Bool

    , _status         :: IO String
    , _progress       :: IO String
    , _artist         :: IO String
    , _album          :: IO String
    , _title          :: IO String

    , _play           :: IO ()
    , _toggle         :: IO ()
    , _stop           :: IO ()
    , _previous       :: IO ()
    , _next           :: IO ()
    }
