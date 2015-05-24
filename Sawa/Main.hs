module Main (main) where


import Ma             (doEveryM)

import System.Process (system)


main :: IO ()
main = doEveryM 8 $ system "xte 'key Control_R'"
