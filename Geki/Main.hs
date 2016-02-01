module Main (main) where


import Control.Concurrent.Async           (mapConcurrently)
import Control.Monad                      (forM_, when)

import Data.List                          ((\\), find, intercalate, isInfixOf, nubBy, union)
import Data.Maybe                         (isJust, catMaybes)

import System.Environment                 (getArgs)
import qualified System.IO.Strict as SIO  (readFile)
import System.Process                     (system)

import Text.Printf                        (printf)


import Common
import qualified DramaBay     as Bay
import qualified DramaNice    as Nic
import qualified IKShow       as IKS
import qualified MyAsianFever as Fev
import qualified MyAsianTV    as MAT


main :: IO ()
main = do
    有:_ <- getArgs
    白   <- fmap read . SIO.readFile $ ぶ "白" :: IO [([String], String)]

    urls <- catMaybes
        <$> mapConcurrently getURL
                [ Bay.url
                , MAT.url
                , Fev.url
                , IKS.url
                , Nic.url
                ]

    let 劇  = filter ((`elem` concatMap fst 白) . _name)
            . concat
            $ zipWith (\us (url, es, ls) -> parseEpisodes url es `concatMap` ls us)
                urls
                [ (Bay.url, Bay.episodes, Bay.links)
                , (MAT.url, MAT.episodes, MAT.links)
                , (Fev.url, Fev.episodes, Fev.links)
                , (IKS.url, IKS.episodes, IKS.links)
                , (Nic.url, Nic.episodes, Nic.links)
                ]

    -- let a = maybe undefined id <$> getURL Nic.url
    -- print =<< a
    -- mapM_ putStrLn . Fev.links =<< a

    劇古 <- r劇
    未   <- r未

    let 劇新 = reverse $ 劇 \\ union 劇古 未

    w劇 劇
    a未 劇新

    notify 劇新 白

    n  <- (<$> r未)
        $ length
        . nubBy (\(Episode _ n1 e1 _) (Episode _ n2 e2 _)
                    -> lookup' n1 白 == lookup' n2 白
                    && e1 == e2)

    todo
    when (n > 0) . putStr $ show n ++ 有

    where
    notify xs wl = forM_ xs $ \(Episode site name ep sub) -> system $ printf
        "notify-send -u low -a %s '【 %02d 】　%s%s'"
        (show site)
        ep
        (name `lookup'` wl)
        (maybe "" (printf "　  %d%%") sub)


    todo = putStr . (++ sep) . intercalate sep . lines =<< SIO.readFile (ぶ "椴")
        where sep = "　"


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
