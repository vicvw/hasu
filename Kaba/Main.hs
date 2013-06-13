module Main where


import Dispatch         (run, watch)
import Options          (getOptions, oCommand, oIgnored, oMain, oOnce)

import Control.Monad    (when)
import System.Directory (getCurrentDirectory)


main :: IO ()
main = do
    (o, r) <- getOptions
    cwd    <- getCurrentDirectory

    let [mfile, mcmd] = map ($ o) [oMain, oCommand]
        igns = oIgnored o
        once = oOnce o

    -- print o
    putStrLn "元"

    maybe (ioError . userError $ "no main file")
          (\file -> do
              when once $ run mcmd r file
              watch cwd igns mcmd r file)
          mfile
