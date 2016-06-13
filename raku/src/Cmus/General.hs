module Cmus.General
    ( cmus
    , isRunning
    , Command (..)
    ) where


import Control.Applicative  ((<$>))

import System.Exit          (ExitCode (..))
import System.Process       (readProcess, readProcessWithExitCode)


isRunning :: IO Bool
isRunning = do
    (code, _, _) <- readProcessWithExitCode "cmus-remote" ["-Q"] ""

    return $ case code of
        ExitSuccess   -> True
        ExitFailure _ -> False


cmus :: Command -> IO (Maybe String)
cmus cmd = do
    running <- isRunning

    if running
    then Just <$> readProcess "cmus-remote" [show cmd] ""
    else return Nothing


instance Show Command where
    show cmd = case cmd of
        Query    -> "-Q"
        Play     -> "--play"
        Toggle   -> "--pause"
        Stop     -> "--stop"
        Previous -> "--prev"
        Next     -> "--next"


data Command
    = Query
    | Play
    | Toggle
    | Stop
    | Previous
    | Next
