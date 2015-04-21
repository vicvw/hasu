{-# LANGUAGE LambdaCase #-}

module Main (main) where


import Volume

import Data.Foldable      (find)
import Data.Maybe         (fromJust)

import System.Environment (getArgs)
import System.Process     (readProcess)


main :: IO ()
main =
    getArgs >>= \case
        [] -> do
            sinks <- sinkInputs
            let s = unlines $ map show sinks

            out <- readProcess "dmenu" dmenuArgs s

            let i = read $ takeWhile (/= ' ') out
            toggleMuteSink . fromJust $ find ((== i) . inputIndex) sinks

        (app:_) ->
            toggleMuteApp app

    where
    dmenuArgs =
        [ "-b" , "-i"
        , "-fn", "Noto Sans CJK JP:size=14"
        , "-nb", "#000000"
        , "-nf", "#ffffff"
        , "-sb", "#ffffff"
        , "-sf", "#000000"
        ]
