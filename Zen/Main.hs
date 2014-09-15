module Main (main) where


import Dzen.Dzen


import Control.Monad  (void)

import System.Process (system)


main :: IO ()
main = do
    print dzen
    void $ system dzen

    where
    dzen = unwords . toArguments $ defaultOptions
        { _timeout        = Just $ Just 2
        , _menuMode       = Just $ Just OHorizontal
        , _titleAlignment = Just ALeft
        , _background     = Just $ RGB 0 0 0
        , _dock           = True
        }
