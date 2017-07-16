{-# LANGUAGE RecordWildCards #-}

module Main (main) where


import Control.Concurrent.Async           (mapConcurrently)
import Control.Monad                      (forM_, when, (<=<))

import Data.List                          ((\\), find, intercalate, isInfixOf, nubBy, union)
import Data.Maybe                         (isJust, catMaybes)

import System.Environment                 (getArgs)
import qualified System.IO.Strict as SIO  (readFile)
import System.Process                     (system)

import Text.Printf                        (printf)


import Nara

import Common
import qualified DramaCool
import qualified HDFree


main :: IO ()
main = do
    arg <- head <$> getArgs
    wht <- getWhitelist
    old <- r劇
    yet <- r未
    drm <- whitelist wht <$> getDramas
        [ DramaCool.spec
        , HDFree.spec
        ]

    let new = reverse $ drm \\ union old yet
    notify new wht

    w劇 drm
    a未 new

    outputTodo
    output arg wht

    where
    getWhitelist  = fmap read . SIO.readFile $ ne "白" :: IO [([String], String)]
    whitelist l   = filter $ (`elem` concatMap fst l) . _name

    getDramas specs = concat . catMaybes <$> (`mapConcurrently` specs)
        (\Spec {..} -> fmap (parseEpisodes url parser <=< links) <$> getUrl url)

    notify xs wl = forM_ xs $ \(Episode site name ep sub) -> system $ printf
        "notify-send -u low -a %s '　%02d %s %s'"
        (show site)
        ep
        (ii "　" "・" sub)
        (name `lookup'` wl)

    outputTodo = putStr . if' null id (++ sep) . intercalate sep . lines =<< SIO.readFile (ne "椴")
        where sep = "　"

    output sep wl = do
        n  <- (<$> r未) $ length
            . nubBy (\(Episode _ n1 e1 _) (Episode _ n2 e2 _)
                        -> lookup' n1 wl == lookup' n2 wl
                        && e1 == e2)

        when (n > 0) . putStrLn $ show n ++ sep


    lookup' x = maybe (error x) snd . find (isJust . find (x `isInfixOf`) . fst)

    w劇 = writeFile  (ne "劇") . show'
    a未 = appendFile (ne "未") . show'

    r劇 = read' <$> SIO.readFile (ne "劇")
    r未 = read' <$> SIO.readFile (ne "未")

    show' :: [Episode] -> String
    show' eps = unlines $ show <$> eps

    read' :: String -> [Episode]
    read' = map read . lines

    ne  = ("/home/v/ぶ/geki/" ++)
