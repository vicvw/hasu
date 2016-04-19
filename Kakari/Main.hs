module Main (main) where


import Data.List.Extra  (splitOn)
import System.Exit      (exitFailure, exitSuccess)
import System.Process   (readProcessWithExitCode)

import Nara


main :: IO ()
main = do
    (_,p,_) <- readProcessWithExitCode "pacman" ["-Qi", "linux"] ""
    (_,u,_) <- readProcessWithExitCode "uname"  ["-r"]           ""

    let p'  = if' ((== 2) . length . head)
                  (\[a, b] -> [a ++ ["0"], b])
                  id
            . split' . drop 2 . dropWhile (/= ':') . (!! 1) $ lines p
        u'  = take 2 $ split' u

    fi (p' == u')
        exitSuccess
        exitFailure

    where
    split' = (splitOn "." <$>) . splitOn "-"
