module Dzen.GdbarInstances where


import Dzen.DzenInstances  (Color)
import Text.Printf    (printf)


data GdbarStyle
    = Outlined
    | Vertical


-- data Color
--     = Hex String
--     | RGB Integer Integer Integer


data GdbarOption
    = Style         GdbarStyle
    | Width         Integer
    | Height        Integer
    | SegmentWidth  Integer
    | SegmentHeight Integer
    | SegmentSpace  Integer
    | Foreground    Color
    | Background    Color
    | Minimum       Integer
    | Maximum       Integer
    | Label         String
    | NoNewline     Bool


instance Show GdbarStyle where
    show style = case style of
      Outlined -> "o"
      Vertical -> "v"


instance Show GdbarOption where
    show option = case option of
        (Style s)          -> showOption "s"   $ show s
        (Width w)          -> showOption "w"   $ show w
        (Height h)         -> showOption "h"   $ show h
        (SegmentWidth sw)  -> showOption "sw"  $ show sw
        (SegmentHeight sh) -> showOption "sh"  $ show sh
        (SegmentSpace ss)  -> showOption "ss"  $ show ss
        (Foreground fg)    -> showOption "fg"  $ show fg
        (Background bg)    -> showOption "bg"  $ show bg
        (Minimum min)      -> showOption "min" $ show min
        (Maximum max)      -> showOption "max" $ show max
        (Label label)      -> showOption "l"   $ show label
        (NoNewline nonl)   -> case nonl of
                                  True  -> showEmptyOption "nonl"
                                  False -> ""
      where
        showEmptyOption = printf "-%s"
        showOption      = printf "-%s %s"


instance Eq GdbarOption where
    (Style         _) == (Style         _) = True
    (Width         _) == (Width         _) = True
    (Height        _) == (Height        _) = True
    (SegmentWidth  _) == (SegmentWidth  _) = True
    (SegmentHeight _) == (SegmentHeight _) = True
    (SegmentSpace  _) == (SegmentSpace  _) = True
    (Foreground    _) == (Foreground    _) = True
    (Background    _) == (Background    _) = True
    (Minimum       _) == (Minimum       _) = True
    (Maximum       _) == (Maximum       _) = True
    (Label         _) == (Label         _) = True
    (NoNewline     _) == (NoNewline     _) = True
    _                 == _                 = False
