module Main (main) where


import Dispatch
import Options


import Control.Applicative  ((<$>), (<|>))
import Control.Monad        (when)
import Data.Maybe           (fromMaybe)
import System.Directory     (getCurrentDirectory)


main :: IO ()
main = do
    (o, r) <- getOptions
    cwd    <- getCurrentDirectory

    let [mfile, mcmd] = ($ o) <$> [oMain, oCommand]
        [files, igns] = ($ o) <$> [oFiles, oIgnored]

        once = oOnce o

        file = fromMaybe "" mfile

    flip (flip . maybe)
        (mfile <|> mcmd)
        (error "no main or command")
        . const $ do
            putStrLn "元\n"
            when once $ run mcmd r file
            watch cwd (files, igns) mcmd r file
