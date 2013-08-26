module Dzen.Dzen
    ( defaultOptions
    , module Dzen.Types
    ) where


import Dzen.Types


defaultOptions :: DzenOptions
defaultOptions = DzenOptions
    { _timeout         = Nothing
    , _menuMode        = Nothing
    , _titleAlignment  = Nothing
    , _slaveAlignment  = Nothing
    , _xPosition       = Nothing
    , _yPosition       = Nothing
    , _width           = Nothing
    , _height          = Nothing
    , _titleWidth      = Nothing
    , _events          = Nothing
    , _lines           = Nothing
    , _font            = Nothing
    , _background      = Nothing
    , _foreground      = Nothing
    , _geometry        = Nothing
    , _expand          = Nothing
    , _titleName       = Nothing
    , _slaveName       = Nothing
    , _screen          = Nothing
    , _updateSim       = False
    , _dock            = False
    }
