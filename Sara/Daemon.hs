module Main (main) where


import Ma             (doEveryM)


import Control.Monad  (void)
import System.Process (system)


main :: IO ()
main = doEveryM 60 . void $ system "sudo pacman -Syw"
