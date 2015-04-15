module Main (main) where


import Ma             (doEveryM)


import Control.Monad  (void)
import System.Process (readProcess)


main :: IO ()
main = doEveryM 8 $ xte ["key Control_R"]


xte :: [String] -> IO ()
xte args = void $ readProcess "xte" args ""
