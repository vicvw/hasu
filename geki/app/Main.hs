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
import qualified DramaLove
import qualified NewAsianTV


main :: IO ()
main = do
    有 <- head <$> getArgs
    白 <- getWhitelist
    古 <- r劇
    未 <- r未
    劇 <- whitelist 白 <$> getDramas
        [ DramaLove.spec
        , NewAsianTV.spec
        ]

    let 新 = reverse $ 劇 \\ union 古 未
    notify 新 白

    w劇 劇
    a未 新

    outputTodo
    output 有 白

    where
    getWhitelist  = fmap read . SIO.readFile $ フ "白" :: IO [([String], String)]
    whitelist l   = filter $ (`elem` concatMap fst l) . _name

    getDramas specs = concat . catMaybes <$> (`mapConcurrently` specs)
        (\Spec {..} -> fmap (parseEpisodes url parser <=< links) <$> getUrl url)

    notify xs wl = forM_ xs $ \(Episode site name ep sub) -> system $ printf
        "notify-send -u low -a %s '　%02d %s %s'"
        (show site)
        ep
        (ii "　" "・" sub)
        (name `lookup'` wl)

    outputTodo = putStr . if' null id (++ sep) . intercalate sep . lines =<< SIO.readFile (フ "椴")
        where sep = "　"

    output sep wl = do
        n  <- (<$> r未) $ length
            . nubBy (\(Episode _ n1 e1 _) (Episode _ n2 e2 _)
                        -> lookup' n1 wl == lookup' n2 wl
                        && e1 == e2)

        when (n > 0) . putStrLn $ show n ++ sep


    lookup' x = maybe (error x) snd . find (isJust . find (x `isInfixOf`) . fst)

    w劇 = writeFile  (フ "劇") . show'
    a未 = appendFile (フ "未") . show'

    r劇 = read' <$> SIO.readFile (フ "劇")
    r未 = read' <$> SIO.readFile (フ "未")

    show' :: [Episode] -> String
    show' eps = unlines $ show <$> eps

    read' :: String -> [Episode]
    read' = map read . lines

    フ  = ("/home/v/ぶ/geki/" ++)
