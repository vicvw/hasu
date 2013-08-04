{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Graphics.Vty.Widgets.All

import Control.Concurrent

import Control.Monad
import qualified Data.Text as T

import System.Process


main :: IO ()
main = do
    e  <- editWidget
    ui <- centered e

    fg <- newFocusGroup
    addToFocusGroup fg e

    c  <- newCollection
    void $ addToCollection c ui fg

    onActivate e $
        -- getEditText >=>
        -- error . ("entered: " ++) . T.unpack
        const $ do
        makeSound
        -- threadDelay $ 1 * 10^6
        -- error "blub"

    runUi c defaultContext


makeSound :: IO ()
-- makeSound = void $
makeSound = void . forkIO $ do
    -- readProcess "mplayer" ["/usr/share/sounds/freedesktop/stereo/complete.oga"] ""
    (_, _, _, pid) <- runInteractiveProcess "mplayer"
        ["/usr/share/sounds/freedesktop/stereo/complete.oga"]
        Nothing
        Nothing

    waitForProcess pid

    return ()
