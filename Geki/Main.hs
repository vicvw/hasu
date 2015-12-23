module Main (main) where


import Common
import qualified DramaBay  as Bay
import qualified MyAsianTV as MAT
import qualified DramaNet  as Net


import Control.Arrow                      (first)
import Control.Concurrent.Async           (mapConcurrently)
import Control.Monad                      (forM_, when)

import Data.List                          ((\\), find, intercalate, isInfixOf, nubBy, union)
import Data.Maybe                         (fromJust, isJust, catMaybes)

import System.Environment                 (getArgs)
import System.IO                          (appendFile, writeFile)
import qualified System.IO.Strict as SIO  (readFile)
import System.Process                     (system)

import Text.Printf                        (printf)


main :: IO ()
main = do
    有:_ <- getArgs
    白   <- fmap read . SIO.readFile $ ぶ "白" :: IO [([String], String)]

    tags <- map getTags <$> catMaybes
        <$> mapConcurrently getURL [Bay.url, MAT.url, Net.url]

    let 劇  = filter ((`elem` concatMap fst 白) . name)
            . concat
            $ zipWith (\ts (url, es, ls) -> parseEpisodes url es `concatMap` ls ts)
                tags
                [ (Bay.url, Bay.episodes, Bay.links)
                , (MAT.url, MAT.episodes, MAT.links)
                , (Net.url, Net.episodes, Net.links)
                ]

    劇古 <- r劇
    未   <- r未

    let 劇新 = reverse $ 劇 \\ union 劇古 未

    w劇 劇
    a未 劇新

    forM_ 劇新 $ \(Episode s n e t) -> system $ printf
        "notify-send -u low -a %s '【 %02d 】　%s%s'"
        (show s)
        e
        (n `lookup'` 白)
        (maybe "" (printf "　  %d%%") t)

    n  <- (<$> r未)
        $ length
        . nubBy (\(Episode _ n1 e1 _) (Episode _ n2 e2 _)
                    -> lookup' n1 白 == lookup' n2 白
                    && e1 == e2)

    putStr . (++ 有) . intercalate 有 . lines =<< SIO.readFile (ぶ "椴")
    when (n > 0) . putStr $ show n ++ 有

    where
    lookup' x = maybe (error x) snd . find (isJust . find (x `isInfixOf`) . fst)


    w劇 = writeFile  (ぶ "劇") . show'
    a未 = appendFile (ぶ "未") . show'

    r劇 = read' <$> SIO.readFile (ぶ "劇")
    r未 = read' <$> SIO.readFile (ぶ "未")

    ぶ  = ("/home/v/ぶ/Geki/" ++)


    show' :: [Episode] -> String
    show' eps = unlines $ show <$> eps

    read' :: String -> [Episode]
    read' = map read . lines
