module Main where


import Dispatch         (run, watch)
import Options          (getOptions, oCommand, oFiles, oIgnored, oMain, oOnce)

import Control.Monad    (when)
import System.Directory (getCurrentDirectory)


main :: IO ()
main = do
    (o, r) <- getOptions
    cwd    <- getCurrentDirectory

    let [mfile, mcmd] = ($ o) `map` [oMain, oCommand]
        [files, igns] = ($ o) `map` [oFiles, oIgnored]
        once = oOnce o

    -- print o
    putStrLn "元\n"

    maybe (ioError . userError $ "no main file")
          (\file -> do
              when once $ run mcmd r file
              watch cwd (files, igns) mcmd r file)
          mfile
