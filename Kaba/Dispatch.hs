module Dispatch
    ( run
    , watch
    ) where


import General
import Handlers


import System.INotify

import Control.Monad          (forever, unless, void, when)
import Control.Concurrent     (threadDelay)

import Data.List              (find)
import Data.Maybe             (fromJust)

import System.FilePath.Posix  (takeExtension)

import Text.Regex.PCRE        ((=~))


watch :: FilePath
      -> ([FilePath], [FilePath])
      -> Maybe Command
      -> Handler
watch dir filesIgns cmd args file = withINotify $ \inotify -> do
    addWatch inotify [Modify] dir $ handle filesIgns cmd args file
    wait

    where
    wait = void . forever . threadDelay $ 10^6


handle :: ([FilePath], [FilePath])
       -> Maybe Command
       -> Arguments
       -> FilePath
       -> Event
       -> IO ()
handle (files, igns) cmd args file (Modified _ mpath) =
    when (null files || isListed) .
    unless isIgnored $
        run cmd args file

    where
    isListed  = path `elem` files
    isIgnored = any (path =~) ignored
    ignored   = ["\\.sw[po]$", "\\.o$", "\\.hi$"] ++ map (++ "$") igns
    path      = fromJust mpath


run :: Maybe Command -> Handler
run mcmd args file =
    maybe (dispatch file args file)
          (\cmd -> custom cmd args file)
          mcmd


dispatch :: FilePath -> Handler
dispatch file
    = fromJust
    $ lookup'
        (takeExtension file)
        handlers

    where
    lookup' ext
        = fmap snd
        . find (any (ext =~) . fst)
