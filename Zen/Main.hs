module Main (main) where


import Dzen.Dzen


main :: IO ()
main = print . toArguments $ defaultOptions
    { _timeout        = Just $ Just 2
    , _menuMode       = Just $ Just OHorizontal
    , _titleAlignment = Just ALeft
    , _background     = Just $ RGB 0 0 0
    , _dock           = True
    }
