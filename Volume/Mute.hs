{-# LANGUAGE LambdaCase #-}

module Main (main) where


import Volume

import Data.Foldable      (find)
import Data.List          (isInfixOf)
import Data.Maybe         (fromJust)

import System.Environment (getArgs)
import System.Process     (readProcess)


main :: IO ()
main =
    getArgs >>= \case
        [] -> do
            sinks <- sinkInputs
            let 黒 = ["C*"]
                示 = unlines
                   . filter (not . (`any` 黒) . flip isInfixOf)
                   $ map show sinks
            putStrLn 示

            out <- readProcess "dmenu" dmenuArgs 示

            let i = read $ takeWhile (/= ' ') out
            toggleMuteSink . fromJust $ find ((== i) . inputIndex) sinks

        (app:_) ->
            toggleMuteApp app

    where
    dmenuArgs =
        [ "-b" , "-i"
        , "-fn", "Noto Sans CJK JP:pixelsize=12"
        , "-nb", "#000000"
        , "-nf", "#ffffff"
        , "-sb", "#ffffff"
        , "-sf", "#000000"
        ]
