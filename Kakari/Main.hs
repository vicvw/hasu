module Main (main) where


import Data.List      (isInfixOf)
import System.Exit    (exitFailure, exitSuccess)
import System.Process (readProcessWithExitCode)


main :: IO ()
main = do
    (_,p,_) <- readProcessWithExitCode "pacman" ["-Qi", "linux"] ""
    (_,u,_) <- readProcessWithExitCode "uname"  ["-r"]           ""

    let pacman = drop 2 . dropWhile (/= ':') . (!! 1) $ lines p

    if pacman `isInfixOf` u
    then exitSuccess
    else exitFailure
