{-# LANGUAGE RecordWildCards #-}

module Dzen.Types
    ( toArguments
    , DzenOptions (..)
    , Orientation (..)
    , Alignment   (..)
    , Color       (..)
    , Direction   (..)
    ) where


-- import Data.Maybe
import Text.Printf (printf)


data DzenOptions = DzenOptions
    { _timeout        :: Maybe (Maybe Integer)
    , _menuMode       :: Maybe (Maybe Orientation)
    , _titleAlignment :: Maybe Alignment
    , _slaveAlignment :: Maybe Alignment
    , _xPosition      :: Maybe Integer
    , _yPosition      :: Maybe Integer
    , _width          :: Maybe Integer
    , _height         :: Maybe Integer
    , _titleWidth     :: Maybe Integer
    , _events         :: Maybe ()
    , _lines          :: Maybe Integer
    , _font           :: Maybe String
    , _background     :: Maybe Color
    , _foreground     :: Maybe Color
    , _geometry       :: Maybe String
    , _expand         :: Maybe Direction
    , _titleName      :: Maybe String
    , _slaveName      :: Maybe String
    , _screen         :: Maybe Integer
    , _updateSim      :: Bool
    , _dock           :: Bool
    }


toArguments :: DzenOptions -> [String]
toArguments (DzenOptions {..}) = concat
    [ maybeEmpty  "-p" (maybe []) _timeout
    , maybeEmpty  "-m" (maybe []) _menuMode
    , maybeEmpty' "-ta"           _titleAlignment
    , maybeEmpty' "-sa"           _slaveAlignment
    , maybeEmpty' "-x"            _xPosition
    , maybeEmpty' "-y"            _yPosition
    , maybeEmpty' "-w"            _width
    , maybeEmpty' "-h"            _height
    , maybeEmpty' "-tw"           _titleWidth
    , maybeEmpty' "-l"            _lines
    , maybeEmpty' "-fn"           _font
    , maybeEmpty' "-bg"           _background
    , maybeEmpty' "-fg"           _foreground
    , maybeEmpty' "-geometry"     _geometry
    , maybeEmpty' "-expand"       _expand
    , maybeEmpty' "-title-name"   _titleName
    , maybeEmpty' "-slave-name"   _slaveName
    , maybeEmpty' "-xs"           _screen
    , ifEmpty     "-u"            _updateSim
    , ifEmpty     "-dock"         _dock
    , const       []              _events
    ]

    where
    maybeEmpty' x  = maybeEmpty x id
    maybeEmpty x f = maybe [] $ (x :) . f (return . show)

    ifEmpty x cond = [ x | cond ]


data Orientation
    = OHorizontal
    | OVertical

instance Show Orientation where
    show orientation = case orientation of
        OHorizontal -> "h"
        OVertical   -> "v"


data Alignment
    = ALeft
    | ACenter
    | ARight

instance Show Alignment where
    show alignment = case alignment of
        ALeft   -> "l"
        ACenter -> "c"
        ARight  -> "r"


data Color
    = Hex String
    | RGB Integer Integer Integer

instance Show Color where
    show color = case color of
        Hex hex   -> hex
        RGB r g b -> toHex r g b

        where
        toHex = printf "#%02x%02x%02x"


data Direction
    = DLeft
    | DRight

instance Show Direction where
    show direction = case direction of
        DLeft  -> "left"
        DRight -> "right"
