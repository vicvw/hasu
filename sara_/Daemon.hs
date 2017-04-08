module Main (main) where


import Ma             (doEveryM)

import System.Process (system)


main :: IO ()
main = doEveryM 60 $ system "sudo pacman -Sy"
-- main = doEveryM 60 $ system "sudo pacman -Syuw --noconfirm"
