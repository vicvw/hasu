module Main (main) where


import Dzen.Dzen


import Control.Monad  (void)
import System.Process (system)


main :: IO ()
main = do
    print dzen
    void $ system dzen

    where
    dzen
        = ("echo ' ' | dzen2 " ++)
        . unwords
        . toArguments
        . foldr1 (.)
            [ timeout     $ Just 2
            , xPosition   995
            , yPosition   1064
            , width       121
            , height      5
            , background  $ RGB 255 127 127
            , foreground  $ RGB 255 255 255
            ]
        $ defaultOptions
