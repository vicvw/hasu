module Dzen.DzenInstances where


import Text.Printf (printf)


data Color
    = Hex String
    | RGB Integer Integer Integer


data Direction
    = DLeft
    | DRight


data Alignment
    = ALeft
    | ACenter
    | ARight


data Orientation
    = Horizontal
    | Vertical


data DzenOption
    = Timeout        Integer
    | MenuMode       Orientation
    | TitleAlignment Alignment
    | SlaveAlignment Alignment
    | XPosition      Integer
    | YPosition      Integer
    | Width          Integer
    | Height         Integer
    | TitleWidth     Integer
    | Events         ()
    | Lines          Integer
    | Font           String
    | Background     Color
    | Foreground     Color
    | Geometry       String
    | Expand         Direction
    | TitleName      String
    | SlaveName      String
    | Screen         Integer
    | UpdateSim      Bool
    | Dock           Bool


instance Show Color where
    show color = case color of
        Hex hex   -> hex
        RGB r g b -> printf "'#%02x%02x%02x'" r g b
        -- RGB r g b -> printf "#%02x%02x%02x" r g b


instance Show Direction where
    show direction = case direction of
        DLeft  -> "left"
        DRight -> "right"


instance Show Alignment where
    show alignment = case alignment of
        ALeft   -> "l"
        ACenter -> "c"
        ARight  -> "r"


instance Show Orientation where
    show orientation = case orientation of
        Horizontal -> "h"
        Vertical   -> "v"


instance Show DzenOption where
    show option = case option of
        (Timeout t)         -> showOption "p"          $ show t
        (MenuMode o)        -> showOption "m"          $ show o
        (TitleAlignment a)  -> showOption "ta"         $ show a
        (SlaveAlignment a)  -> showOption "sa"         $ show a
        (XPosition x)       -> showOption "x"          $ show x
        (YPosition y)       -> showOption "y"          $ show y
        (Width w)           -> showOption "w"          $ show w
        (Height h)          -> showOption "h"          $ show h
        (TitleWidth tw)     -> showOption "tw"         $ show tw
        (Lines n)           -> showOption "l"          $ show n
        (Font font)         -> showOption "fn"         $ show font
        (Background c)      -> showOption "bg"         $ show c
        (Foreground c)      -> showOption "fg"         $ show c
        (Geometry geometry) -> showOption "geometry"   $ show geometry
        (Expand direction)  -> showOption "expand"     $ show direction
        (TitleName name)    -> showOption "title-name" $ show name
        (SlaveName name)    -> showOption "slave-name" $ show name
        (Screen n)          -> showOption "xs"         $ show n
        (UpdateSim update)  -> if update
                               then showEmptyOption "u"
                               else ""
        (Dock dock)         -> if dock
                               then showEmptyOption "dock"
                               else ""
        (Events _)          -> undefined

      where
      showEmptyOption = printf "-%s"
      showOption      = printf "-%s %s"


instance Eq DzenOption where
    (Timeout        _) == (Timeout        _) = True
    (MenuMode       _) == (MenuMode       _) = True
    (TitleAlignment _) == (TitleAlignment _) = True
    (SlaveAlignment _) == (SlaveAlignment _) = True
    (XPosition      _) == (XPosition      _) = True
    (YPosition      _) == (YPosition      _) = True
    (Width          _) == (Width          _) = True
    (Height         _) == (Height         _) = True
    (TitleWidth     _) == (TitleWidth     _) = True
    (Lines          _) == (Lines          _) = True
    (Font           _) == (Font           _) = True
    (Background     _) == (Background     _) = True
    (Foreground     _) == (Foreground     _) = True
    (Geometry       _) == (Geometry       _) = True
    (Expand         _) == (Expand         _) = True
    (TitleName      _) == (TitleName      _) = True
    (SlaveName      _) == (SlaveName      _) = True
    (Screen         _) == (Screen         _) = True
    (UpdateSim      _) == (UpdateSim      _) = True
    (Dock           _) == (Dock           _) = True
    (Events         _) == (Events         _) = True
    _                  == _                  = False
