module Main where


import Dispatch

import Control.Monad      (forever, forM_, unless, void)
import Control.Concurrent (threadDelay)

import Data.Maybe         (fromJust)

import System.Cmd         (system)
import System.INotify     (addWatch, withINotify, Event (Modified), EventVariety (Modify))
import System.Environment (getArgs)
import System.Directory   (getCurrentDirectory)

import Text.Regex.PCRE    ((=~))


main = do
    args  <-  getArgs
    cwd   <-  getCurrentDirectory
    -- print cwd
    let main = head args
    watch main cwd


watch :: FilePath -> FilePath -> IO ()
watch main dir = withINotify $ \inotify -> do
    addWatch inotify [Modify] dir $ handle main
    wait


handle :: FilePath -> Event -> IO ()
handle f (Modified _ path) =
    unless (isTemp $ fromJust path) $
        run f

    where
    isTemp f = any (f =~) tempList

    tempList = ["\\.swp"]


wait = forever . threadDelay $ 10^6
