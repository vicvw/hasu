{-# LANGUAGE LambdaCase #-}

module Main (main) where


import Volume

import Data.Foldable      (find)
import Data.List          (isInfixOf, nub)
import Data.Maybe         (fromJust)

import System.Environment (getArgs)
import System.Process     (readProcess)


main :: IO ()
main =
    getArgs >>= \case
        [] -> do
            sinks <- sinkInputs
            out   <- readProcess "dmenu" dmenuArgs $ format sinks
            toggle out sinks

        [n@[_]] ->
            toggleMuteSink =<< (!! read n) . blacklist <$> sinkInputs

        apps ->
            mapM_ toggleMuteApp $ nub apps

    where
    format = unlines . map show . blacklist

    toggle out = toggleMuteSink . fromJust . find ((== i) . inputIndex)
        where
        i = read $ takeWhile (/= ' ') out

    blacklist = filter (not . (`any` black) . flip isInfixOf . inputName)
        where
        black = ["C*"]

    dmenuArgs =
        [ "-b" , "-i"
        , "-fn", "Noto Sans CJK JP:pixelsize=12"
        , "-nb", "#000000"
        , "-nf", "#ffffff"
        , "-sb", "#ffffff"
        , "-sf", "#000000"
        ]
